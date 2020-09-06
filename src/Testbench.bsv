
package Testbench;

  import Tage_predictor   :: *;    //Tage predictor module as per algorithm
  import Type_TAGE        :: *;    //Types declarations
  import Utils            :: *;    //Display of current_cycle
  import RegFile          :: *;    //For trace files
  import Vector           :: *;    //for performance counters

 
  `include "parameter.bsv"         // for traceSize that is parameterized.


  function UpdationPacket get_updation_pkt(PredictionPacket t_pred_pkt1, Bit#(1) t_actual_outcome);

    //for generating updation packet after obtaining branch instruction outcome

    UpdationPacket t_upd_pkt = unpack(0);
    t_upd_pkt.mispred = ( t_actual_outcome == t_pred_pkt1.pred ) ? 1'b0 : 1'b1;  //misprediction check
    t_upd_pkt.actualOutcome = t_actual_outcome;
    t_upd_pkt.bimodalindex = t_pred_pkt1.bimodalindex;
    t_upd_pkt.tagTableindex = t_pred_pkt1.tagTableindex;
    t_upd_pkt.tableTag= t_pred_pkt1.tableTag;
    t_upd_pkt.uCtr = t_pred_pkt1.uCtr;
    t_upd_pkt.ctr = t_pred_pkt1.ctr;
    t_upd_pkt.ghr = t_pred_pkt1.ghr;
    t_upd_pkt.phr = t_pred_pkt1.phr;
    t_upd_pkt.tableNo = t_pred_pkt1.tableNo;
    t_upd_pkt.altpred = t_pred_pkt1.altpred;
    t_upd_pkt.pred = t_pred_pkt1.pred;

    return t_upd_pkt;

  endfunction


  module mkTestbench(Empty);

    //trace files containing branch addresses and outcomes
    RegFile#(Bit#(22), Bit#(64)) branches                      <-  mkRegFileFullLoad("trace_files/traces_br.hex");
    RegFile#(Bit#(22), Bit#(1)) actualOutcome                  <-  mkRegFileFullLoad("trace_files/traces_outcome.hex");

    //Based on TAGE predictor design
    Tage_predictor_IFC predictor                               <-  mkTage_predictor;
    Reg#(PredictionPacket) pred_pkt                            <-  mkReg(unpack(0));
    Reg#(UpdationPacket) upd_pkt                               <-  mkReg(unpack(0));

    //program flow control register
    Reg#(Bit#(22)) ctr                                         <-  mkReg(0);

    //Performance monitoring counters
    Reg#(Int#(32)) correct                                     <-  mkReg(0);
    Reg#(Int#(32)) incorrect                                   <-  mkReg(0);
    Vector#(5, Reg#(TableCounters)) table_ctr                  <-  replicateM(mkReg(unpack(0)));

    //performance monitoring counter updation
    function Action table_counters(TableNo tableno, Misprediction mispred);

      action
        if (mispred == 1'b0) begin  //increment correct prediction counter of corresponding table if there is no misprediction
          case (tableno)
            3'b000      : table_ctr[0].predictionCtr <= table_ctr[0].predictionCtr + 1;
            3'b001      : table_ctr[1].predictionCtr <= table_ctr[1].predictionCtr + 1;
            3'b010      : table_ctr[2].predictionCtr <= table_ctr[2].predictionCtr + 1;
            3'b011      : table_ctr[3].predictionCtr <= table_ctr[3].predictionCtr + 1;
            3'b100      : table_ctr[4].predictionCtr <= table_ctr[4].predictionCtr + 1;
          endcase
        end
        else begin                  //increment incorrect prediction counter of corresponding table if there is a misprediction
          case (tableno)
            3'b000      : table_ctr[0].mispredictionCtr <= table_ctr[0].mispredictionCtr + 1;
            3'b001      : table_ctr[1].mispredictionCtr <= table_ctr[1].mispredictionCtr + 1;
            3'b010      : table_ctr[2].mispredictionCtr <= table_ctr[2].mispredictionCtr + 1;
            3'b011      : table_ctr[3].mispredictionCtr <= table_ctr[3].mispredictionCtr + 1;
            3'b100      : table_ctr[4].mispredictionCtr <= table_ctr[4].mispredictionCtr + 1;
          endcase
        end
      endaction
    endfunction


    rule rl_display(ctr >= 0);      //display rule for displaying the current cycle
      `ifdef DISPLAY == 1
        $display("Entered Display rule ");
        $display("\n\n=====================================================================================================================================================");
        $display("\nCycle %d   Ctr %d",cur_cycle, ctr);
      `endif
    endrule


    //execute this at the start as well as there is misprediction (inorder to start over)
    rule rl_initial(ctr == 0 || upd_pkt.mispred == 1'b1 );
      `ifdef DISPLAY
        $display("rule 1");
        $display("\nInitialisation of PC and GHR. GHR has value 0 during initial stage.");
      `endif
      let pc = branches.sub(ctr);
      `ifdef DISPLAY
        $display("\nInitial Program Counter =  %h",pc); 
      `endif
      predictor.computePrediction(pc);
      ctr <= ctr + 1;
      upd_pkt <= unpack(0);
    endrule

    rule rl_comp_pred_upd (ctr < `traceSize && ctr > 0 && upd_pkt.mispred == 1'b0);
      
      PredictionPacket t_pred_pkt = unpack(0);
      UpdationPacket t_u_pkt = unpack(0);
      let pc = branches.sub(ctr);
      
      t_pred_pkt = predictor.output_packet();
      `ifdef DISPLAY
        $display("\nProgram Counter =  %h", branches.sub(ctr-1));
        $display("Prediction = %b", t_pred_pkt.pred);
      `endif
      t_u_pkt = get_updation_pkt(t_pred_pkt, actualOutcome.sub((ctr-1)));
      `ifdef DISPLAY  
        $display("Outcome assigned to Updation_Packet = %b", t_u_pkt.actualOutcome);
      `endif
      upd_pkt <= get_updation_pkt(t_pred_pkt, actualOutcome.sub((ctr-1)));
      predictor.updateTablePred(t_u_pkt);

      //updating the performance monitoring counters based on the misprediction result obtained in the current cycle
      table_counters(t_u_pkt.tableNo, t_u_pkt.mispred);
      if(t_u_pkt.mispred == 1'b1) begin

        ctr <= ctr;  /* update ctr to the current ctr so that the prediction
        can be done from the current cycle which mispredicted the previous branch */

        incorrect <= incorrect + 1; //increment performance counter based on this

      end
      else begin

        predictor.computePrediction(pc); //compute prediction for the current PC if there is no misprediction

        ctr <= ctr + 1; /* update ctr to the next ctr so that the prediction
        can be done from the next cycle since there is no misprediction */

        correct <= correct + 1;  //increment performance counter based on this
      end
    endrule

    rule end_simulation(ctr == `traceSize);
      $display("Result:%d,%d", correct, incorrect);

      `ifdef DISPLAY
        // $display("Incorrect = %d      Correct = %d",incorrect,correct);
        $display("\nBimodal Table \n", fshow(table_ctr[0]));
        $display("\nTable 1\n", fshow(table_ctr[1]));
        $display("\nTable 2 \n", fshow(table_ctr[2]));
        $display("\nTable 3 \n", fshow(table_ctr[3]));
        $display("\nTable 4 \n", fshow(table_ctr[4]));
      `endif
      $finish(0);

    endrule

  endmodule
endpackage
