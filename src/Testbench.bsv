package Testbench;


import Tage_predictor :: *; //Tage predictor module as per design  
import Type_TAGE      :: *;      //Types declarations
import Utils          :: *;          //Display of current_cycle 
import RegFile        :: *;        //for trace files
import Vector         :: *;         //for performance counters

`include "parameter.bsv"    //for traceSize that is parameterized.


function Updation_Packet get_updation_pkt(Prediction_Packet t_pred_pkt1, ACTUAL_OUTCOME t_actual_outcome);

    //for generating updation packet after obtaining branch instruction outcome

    Updation_Packet t_upd_pkt = unpack(0);
    t_upd_pkt.mispred = ( t_actual_outcome == t_pred_pkt1.pred ) ? 1'b0 : 1'b1; //misprediction check
    t_upd_pkt.actual_outcome = t_actual_outcome;
    t_upd_pkt.bimodalindex = t_pred_pkt1.bimodalindex;
    t_upd_pkt.index = t_pred_pkt1.index;
    t_upd_pkt.comp_tag1_table = t_pred_pkt1.comp_tag1_table;
    t_upd_pkt.comp_tag2_table = t_pred_pkt1.comp_tag2_table;
    t_upd_pkt.usefulbits = t_pred_pkt1.usefulbits;
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
  RegFile#(Int#(32), PC) branches                              <-  mkRegFileLoad("reg_files/traces_br.hex", 0, `traceSize-1);
  RegFile#(Int#(32), ACTUAL_OUTCOME) actual_outcome            <-  mkRegFileLoad("reg_files/traces_outcome.hex", 0, `traceSize-1);

  //Based on TAGE predictor design
  Tage_predictor_IFC predictor                                 <-  mkTage_predictor; //Tage predictor module instantiation
  Reg#(Prediction_Packet) pred_pkt                             <-  mkReg(unpack(0)); //prediction packet
  Reg#(Updation_Packet) upd_pkt                                <-  mkReg(unpack(0)); //updation packet

  //Performance monitoring counters
  Reg#(Int#(32)) correct                                       <-  mkReg(0);
  Reg#(Int#(32)) incorrect                                     <-  mkReg(0);
  Vector#(TAdd#(`NUMTAGTABLES,1), Reg#(Table_ctrs)) table_ctr  <-  replicateM(mkReg(unpack(0)));

  //program flow control register
  Reg#(Int#(32)) ctr                                           <-  mkReg(0);

  //performance monitoring counter updation
  function Action table_ctrs(TABLENO tableno, MISPRED mispred);
    action
      if (mispred == 1'b0) begin  //increment correct prediction counter of corresponding table if there is no misprediction
        case (tableno)
          3'b000  : table_ctr[0].prediction_ctr <= table_ctr[0].prediction_ctr + 1;
          3'b001  : table_ctr[1].prediction_ctr <= table_ctr[1].prediction_ctr + 1;
          3'b010  : table_ctr[2].prediction_ctr <= table_ctr[2].prediction_ctr + 1;
          3'b011  : table_ctr[3].prediction_ctr <= table_ctr[3].prediction_ctr + 1;
          3'b100  : table_ctr[4].prediction_ctr <= table_ctr[4].prediction_ctr + 1;
        endcase
      end
      else begin                  //increment incorrect prediction counter of corresponding table if there is misprediction
        case (tableno)            
          3'b000  : table_ctr[0].misprediction_ctr <= table_ctr[0].misprediction_ctr + 1;
          3'b001  : table_ctr[1].misprediction_ctr <= table_ctr[1].misprediction_ctr + 1;
          3'b010  : table_ctr[2].misprediction_ctr <= table_ctr[2].misprediction_ctr + 1;
          3'b011  : table_ctr[3].misprediction_ctr <= table_ctr[3].misprediction_ctr + 1;
          3'b100  : table_ctr[4].misprediction_ctr <= table_ctr[4].misprediction_ctr + 1;
        endcase
      end
    endaction
  endfunction

  rule rl_display(ctr > 0);       //display rule for displaying the current cycle

    $display("Entered Display rule ");
    $display("\n\n======================================");
    $display("\nCycle %d   Ctr %d",cur_cycle, ctr);
  
  endrule

  rule rl_initial(ctr == 0 || upd_pkt.mispred == 1'b1 );

    //$display("rule 1");
    //$display("\nInitialisation of PC and GHR. GHR has value 0 during initial stage.");

    let pc = branches.sub(ctr);    //branch address from trace file initialised to pc
    //$display("\nInitial Program Counter =  %h",pc);

    predictor.computePrediction(pc);//Giving PC as input to TAGE for computing prediction
    
    ctr <= ctr + 1;                //updating control register.
    
    upd_pkt <= unpack(0);          //
  endrule

  rule rl_comp_pred_upd (ctr < `traceSize && ctr > 0 && upd_pkt.mispred == 1'b0);

    Prediction_Packet t_pred_pkt = unpack(0);
    Updation_Packet t_u_pkt = unpack(0);
    let pc = branches.sub(ctr);
    
    t_pred_pkt = predictor.output_packet();
    //$display("\nProgram Counter =  %h", branches.sub(ctr-1));
    //$display("Prediction = %b", t_pred_pkt.pred);
    t_u_pkt = get_updation_pkt(t_pred_pkt, actual_outcome.sub((ctr-1)));
    //$display("Outcome assigned to Updation_Packet = %b", t_u_pkt.actual_outcome);
    upd_pkt <= get_updation_pkt(t_pred_pkt, actual_outcome.sub((ctr-1)));
    predictor.updateTablePred(t_u_pkt);
    table_ctrs(t_u_pkt.tableNo, t_u_pkt.mispred);

    if(t_u_pkt.mispred == 1'b1) begin
      ctr <= ctr;
      incorrect <= incorrect + 1;
    end
    else begin
      predictor.computePrediction(pc);
      ctr <= ctr + 1;
      correct <= correct + 1;
    end

  endrule

  rule end_simulation(ctr == `traceSize);
    $display("Result:%d,%d", correct, incorrect);
    // $display("Incorrect = %d      Correct = %d",incorrect,correct);
    $display("\nBimodal Table \n", fshow(table_ctr[0]));
    $display("\nTable 1\n", fshow(table_ctr[1]));
    $display("\nTable 2 \n", fshow(table_ctr[2]));
    $display("\nTable 3 \n", fshow(table_ctr[3]));
    $display("\nTable 4 \n", fshow(table_ctr[4]));
    $finish(0);
  endrule

endmodule

endpackage