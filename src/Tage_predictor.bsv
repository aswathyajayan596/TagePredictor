package Tage_predictor;

    import Utils :: *;
    import Type_TAGE :: *;
    import RegFile :: *;

    `include "parameter.bsv"


    interface Tage_predictor_IFC;
        method Action computePrediction(PC pc);                                 //Indexing Table,Tag Computation, Comparison of Tag, Obtaining Prediction
        method Action updateTablePred(Updation_Packet upd_pkt);
        method Prediction_Packet output_packet();
    endinterface

    (*synthesize*)
    module mkTage_predictor(Tage_predictor_IFC);

        let bimodal_max = fromInteger(`BIMODALSIZE-1);
        let table_max = fromInteger(`TABLESIZE-1);

        Reg#(GHR) ghr <- mkReg(0);                                  //internal register to store GHR
        Reg#(Prediction_Packet) pred_pkt <- mkReg(unpack(0));  //output - index, tag1 & 2, usefulbits,ctr, ghr,prediction, tableNo, altpred initialised to 0
        RegFile#(BIMODALINDEX, Bimodal_Entry) bimodal <- mkRegFile(0, bimodal_max);
        RegFile#(INDEX, TagEntry) table_0 <- mkRegFile(0, table_max);
        RegFile#(INDEX, TagEntry) table_1 <- mkRegFile(0, table_max);
        RegFile#(INDEX, TagEntry) table_2 <- mkRegFile(0, table_max);
        RegFile#(INDEX, TagEntry) table_3 <- mkRegFile(0, table_max);
        Reg#(PHR) phr <- mkReg(0);

        RWire#(Updation_Packet) rw_upd_pkt <- mkRWire();
        RWire#(Bit#(1)) rw_pred <- mkRWire();
        RWire#(Bit#(1)) upd_pkt_recvd <- mkRWire();
        Wire#(PC) w_pc <- mkWire();



        rule rl_update_GHR;
            let t_ghr = ghr;
            let t_phr = phr;
            let updateRecvd = fromMaybe(0,upd_pkt_recvd.wget());
            let t_u_pkt = fromMaybe (?, rw_upd_pkt.wget());
            if( updateRecvd == 1'b1 && t_u_pkt.mispred == 1'b1) begin
                t_u_pkt.ghr = (t_u_pkt.ghr >> 1);
                if(t_u_pkt.actual_outcome == 1)
                    t_ghr = (t_u_pkt.ghr << 1) + 131'b1;
                else
                    t_ghr = (t_u_pkt.ghr << 1);
                t_phr = (t_u_pkt.phr >> 1);
            end
            else if(updateRecvd == 1'b1 && t_u_pkt.mispred == 1'b0) begin
                t_ghr = t_u_pkt.ghr;
                if(t_u_pkt.actual_outcome == 1)
                    t_ghr = (t_u_pkt.ghr << 1) + 131'b1;
                else
                    t_ghr = (t_u_pkt.ghr << 1);
                t_phr = t_u_pkt.phr;
            end
            else begin
                let pred = fromMaybe(?,rw_pred.wget());
                //let w_pc = fromMaybe(?,rw_pc.wget());
                //$display("PC = %h", w_pc);
                if(pred == 1'b1)
                    t_ghr = ( t_ghr  << 1 ) + 131'b1;
                else
                    t_ghr = ( t_ghr  << 1 );
            end

            t_phr = (t_phr << 1);
            if(w_pc[2] == 1'b1) begin
                t_phr = t_phr + 32'b1;
            end
            //$display("GHR after updation: %b",t_ghr);
            //$display("PHR after updation: %b",t_phr);

            ghr <= t_ghr;
            phr <= t_phr;
        endrule


        method Action computePrediction(PC pc);
            // Tag comp_tag10_table, comp_tag11_table, comp_tag20_table, comp_tag21_table;
            Tag comp_tag[4];
            // Tag comp_tag10_table 0;
            //      Tag comp_tag11_table 0;     //tag1
            // Tag comp_tag20_table 0;
            //      Tag comp_tag21_table 0; //tag2
            BIMODALINDEX bimodal_index;
            INDEX index[4];
            Prediction_Packet t_pred_pkt = unpack(0);
            t_pred_pkt.phr = phr;
            t_pred_pkt.phr = (t_pred_pkt.phr << 1);
            if(pc[2] == 1'b1)
                t_pred_pkt.phr = t_pred_pkt.phr + 32'b1;

            //$display("\nGHR before prediction = %h",ghr);

            //$display("\n\nPrediction Packet of last Prediction",fshow(pred_pkt), cur_cycle);

            //$display("Calculating Index..... ");


            bimodal_index = truncate(compFoldIndex(pc,ghr,t_pred_pkt.phr,3'b000));
            t_pred_pkt.bimodalindex = bimodal_index;

            for(Integer i = 0; i<=3; i=i+1) begin
                Bit#(3) tNo = fromInteger(i+1);
                index[i] = truncate(compFoldIndex(pc,ghr,t_pred_pkt.phr,tNo));
                t_pred_pkt.index[i] = index[i];
                if(i<2) begin
                    comp_tag[i] = tagged Tag2 truncate(compFoldTag(pc,ghr,tNo));
                    t_pred_pkt.comp_tag1_table[0] = comp_tag[0];
                    t_pred_pkt.comp_tag1_table[1] = comp_tag[1];
                end
                else begin
                    comp_tag[i] = tagged Tag1 truncate(compFoldTag(pc,ghr,tNo));
                    t_pred_pkt.comp_tag2_table[0] = comp_tag[2];
                    t_pred_pkt.comp_tag2_table[1] = comp_tag[3];
                end
            end

            // for(Integer i = 0, j=0; i < 4)

            if(table_3.sub(index[3]).tag == comp_tag[3]) begin      //comparing tag and computed tag T4
                t_pred_pkt.pred = table_3.sub(index[3]).ctr[2];   //ctr[2]
                t_pred_pkt.ctr[4] = table_3.sub(index[3]).ctr;
                t_pred_pkt.tableNo = 3'b100;
                if(table_2.sub(index[2]).tag == comp_tag[2]) begin    // alternate table as lower history tables
                    t_pred_pkt.altpred = table_2.sub(index[2]).ctr[2];
                    let alt_tableNo = 3'b011;
                end
                else if(table_1.sub(index[1]).tag == comp_tag[1]) begin
                    t_pred_pkt.altpred = table_1.sub(index[1]).ctr[2];
                    let alt_tableNo = 3'b010;
                end
                else if(table_0.sub(index[0]).tag == comp_tag[0]) begin
                    t_pred_pkt.altpred = table_0.sub(index[0]).ctr[2];
                    let alt_tableNo = 3'b001;
                end
                else begin
                    t_pred_pkt.altpred = bimodal.sub(bimodal_index).ctr[1];
                    let alt_tableNo = 3'b000;
                end
            end
            else if(table_2.sub(index[2]).tag == comp_tag[2]) begin          //comparing tag and computed tag T3
                t_pred_pkt.pred = table_2.sub(index[2]).ctr[2];
                t_pred_pkt.ctr[3] = table_2.sub(index[2]).ctr;
                t_pred_pkt.tableNo = 3'b011;
                if(table_1.sub(index[1]).tag == comp_tag[1]) begin
                    t_pred_pkt.altpred = table_1.sub(index[1]).ctr[2];
                    let alt_tableNo = 3'b010;
                end
                else if(table_0.sub(index[0]).tag == comp_tag[0]) begin
                    t_pred_pkt.altpred = table_0.sub(index[0]).ctr[2];
                    let alt_tableNo = 3'b001;                                                 // alternate table as lower history tables
                end
            else begin
                    t_pred_pkt.altpred = bimodal.sub(bimodal_index).ctr[1];
                    let alt_tableNo = 3'b000;
                end
            end
            else if(table_1.sub(index[1]).tag == comp_tag[1]) begin          //comparing tag and computed tag T2
                t_pred_pkt.pred = table_1.sub(index[1]).ctr[2];
                t_pred_pkt.ctr[2] = table_1.sub(index[1]).ctr;
                t_pred_pkt.tableNo = 3'b010;
                if(table_0.sub(index[0]).tag == comp_tag[0]) begin
                    t_pred_pkt.altpred = table_0.sub(index[0]).ctr[2];
                    let alt_tableNo = 3'b001;                                                   // alternate table as lower history tables
                end
            else begin
                    t_pred_pkt.altpred = bimodal.sub(bimodal_index).ctr[1];
                    let alt_tableNo = 3'b000;
                end
            end
            else if(table_0.sub(index[0]).tag == comp_tag[0]) begin                          //comparing tag and computed tag T1
                t_pred_pkt.pred = table_0.sub(index[0]).ctr[2];
                t_pred_pkt.tableNo = 3'b001;
                t_pred_pkt.ctr[1] = table_0.sub(index[0]).ctr;
                t_pred_pkt.altpred = bimodal.sub(bimodal_index).ctr[1];
                let alt_tableNo = 3'b000;                   // alternate table as lower history tables
            end
            else begin
                t_pred_pkt.pred = bimodal.sub(bimodal_index).ctr[1];
                t_pred_pkt.ctr[0] = zeroExtend(bimodal.sub(bimodal_index).ctr);
                t_pred_pkt.altpred = bimodal.sub(bimodal_index).ctr[1];
                t_pred_pkt.tableNo = 3'b000;
                let alt_tableNo = 3'b000;
            end

            t_pred_pkt.ghr = ghr;
            rw_pred.wset(t_pred_pkt.pred);
            //rw_pc.wset(pc);
            w_pc<=pc;

            if(t_pred_pkt.pred == 1'b1)
                t_pred_pkt.ghr = ( t_pred_pkt.ghr  << 1 ) + 131'b1;
            else
                t_pred_pkt.ghr = ( t_pred_pkt.ghr  << 1 );

            pred_pkt <= t_pred_pkt;

            //  ghr <= t_pred_pkt.ghr;
            //$display("Current PC = %b", pc);
            //$display("\nphr = %b",t_pred_pkt.phr);
            //$display("\nPrediction Packet of current Prediction ", fshow(t_pred_pkt), cur_cycle);
            //$display("Prediction over....");
        endmethod


        method Action updateTablePred(Updation_Packet upd_pkt);

            rw_upd_pkt.wset(upd_pkt);
            upd_pkt_recvd.wset(1'b1);


            let ind0 = upd_pkt.bimodalindex;
            let ind1 = upd_pkt.index[0];
            let ind2 = upd_pkt.index[1];
            let ind3 = upd_pkt.index[2];
            let ind4 = upd_pkt.index[3];
            ACTUAL_OUTCOME outcome = upd_pkt.actual_outcome;

            Bimodal_Entry   t_bimodal = bimodal.sub(ind0);
            TagEntry        t_table_0 = table_0.sub(ind1);
            TagEntry        t_table_1 = table_1.sub(ind2);
            TagEntry        t_table_2 = table_2.sub(ind3);
            TagEntry        t_table_3 = table_3.sub(ind4);

            //$display("\n\nUpdation Packet\n",fshow(upd_pkt));
            //$display("Updation Packet Table Number = %b",upd_pkt.tableNo);
            //$display("GHR = %h", upd_pkt.ghr );

            if(upd_pkt.pred != upd_pkt.altpred) begin                                                                   //updation of provider component's u count
                case (upd_pkt.tableNo)
                    3'b001  :  t_table_0.ubit = (upd_pkt.mispred == 1'b0) ? (t_table_0.ubit + 2'b1) : (t_table_0.ubit - 2'b1);
                    3'b010  :  t_table_1.ubit = (upd_pkt.mispred == 1'b0) ? (t_table_1.ubit + 2'b1) : (t_table_1.ubit - 2'b1);
                    3'b011  :  t_table_2.ubit = (upd_pkt.mispred == 1'b0) ? (t_table_2.ubit + 2'b1) : (t_table_2.ubit - 2'b1);
                    3'b100  :  t_table_3.ubit = (upd_pkt.mispred == 1'b0) ? (t_table_3.ubit + 2'b1) : (t_table_3.ubit - 2'b1);
                endcase
            end

            case (upd_pkt.tableNo)
                3'b000  :   begin
                                if (upd_pkt.actual_outcome == 1'b1)                             //updating the provider component prediction counter
                                    t_bimodal.ctr = (t_bimodal.ctr < 2'b11) ? (t_bimodal.ctr + 2'b1) : 2'b11;
                                else
                                    t_bimodal.ctr = (t_bimodal.ctr > 2'b00) ? (t_bimodal.ctr - 2'b1) : 2'b00;
                            end
                3'b001  :   begin
                                if (upd_pkt.actual_outcome == 1'b1)
                                    t_table_0.ctr = (t_table_0.ctr < 3'b111 )?(t_table_0.ctr + 3'b1): 3'b111;
                                else
                                    t_table_0.ctr = (t_table_0.ctr > 3'b000 )?(t_table_0.ctr - 3'b1): 3'b000;
                            end
                3'b010  :   begin
                                if (upd_pkt.actual_outcome == 1'b1)
                                    t_table_1.ctr = (t_table_1.ctr < 3'b111 )?(t_table_1.ctr + 3'b1): 3'b111;
                                else
                                    t_table_1.ctr = (t_table_1.ctr > 3'b000 )?(t_table_1.ctr - 3'b1): 3'b000;
                            end
                3'b011  :   begin
                                if (upd_pkt.actual_outcome == 1'b1)
                                    t_table_2.ctr = (t_table_2.ctr < 3'b111 )?(t_table_2.ctr + 3'b1): 3'b111;
                                else
                                    t_table_2.ctr = (t_table_2.ctr > 3'b000 )?(t_table_2.ctr - 3'b1): 3'b000;
                            end
                3'b100  :   begin
                                if (upd_pkt.actual_outcome == 1'b1)
                                    t_table_3.ctr = (t_table_3.ctr < 3'b111 )?(t_table_3.ctr + 3'b1): 3'b111;
                                else
                                    t_table_3.ctr = (t_table_3.ctr > 3'b000 )?(t_table_3.ctr - 3'b1): 3'b000;
                            end
            endcase

            if(upd_pkt.mispred == 1'b1) begin           //allocation of new entries if there is a misprediction
                
                case (upd_pkt.tableNo)
                    3'b000  :   begin //all u>0
                                    if (t_table_0.ubit != 2'b0 && t_table_1.ubit != 2'b0 && t_table_2.ubit != 2'b0 && t_table_3.ubit != 2'b0)
                                    begin
                                        t_table_0.ubit = t_table_0.ubit - 2'b1;
                                        t_table_1.ubit = t_table_1.ubit - 2'b1;
                                        t_table_2.ubit = t_table_2.ubit - 2'b1;
                                        t_table_3.ubit = t_table_3.ubit - 2'b1;
                                    end
                                    else begin
                                        if (t_table_3.ubit == 2'b0) begin      //one u=0 then allocate entry
                                            t_table_3.ubit = 2'b0;
                                            t_table_3.tag = upd_pkt.comp_tag2_table[1];
                                            t_table_3.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit == 2'b0) begin
                                            t_table_2.ubit = 2'b0;
                                            t_table_2.tag = upd_pkt.comp_tag2_table[0];
                                            t_table_2.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit != 2'b0 && t_table_1.ubit == 2'b0) begin
                                            t_table_1.ubit = 2'b0;
                                            t_table_1.tag = upd_pkt.comp_tag1_table[1];
                                            t_table_1.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit != 2'b0 && t_table_1.ubit != 2'b0 && t_table_0.ubit == 2'b0) begin
                                            t_table_0.ubit = 2'b0;
                                            t_table_0.tag = upd_pkt.comp_tag1_table[0];
                                            t_table_0.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                    end
                                end
                    3'b001  :   begin
                                    if (t_table_1.ubit != 2'b0 && t_table_2.ubit != 2'b0 && t_table_3.ubit != 2'b0) begin
                                        t_table_1.ubit = t_table_1.ubit - 2'b1;
                                        t_table_2.ubit = t_table_2.ubit - 2'b1;
                                        t_table_3.ubit = t_table_3.ubit - 2'b1;
                                    end
                                    else begin
                                        if (t_table_3.ubit == 2'b0) begin      //one u=0 then allocate entry
                                            t_table_3.ubit = 2'b0;
                                            t_table_3.tag = upd_pkt.comp_tag2_table[1];
                                            t_table_3.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit == 2'b0) begin
                                            t_table_2.ubit = 2'b0;
                                            t_table_2.tag = upd_pkt.comp_tag2_table[0];
                                            t_table_2.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit != 2'b0 && t_table_1.ubit == 2'b0) begin
                                            t_table_1.ubit = 2'b0;
                                            t_table_1.tag = upd_pkt.comp_tag1_table[1];
                                            t_table_1.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                    end
                                end
                    3'b010  :   begin
                                    if (t_table_2.ubit != 2'b0 && t_table_3.ubit != 2'b0) begin
                                        t_table_2.ubit = t_table_2.ubit - 2'b1;
                                        t_table_3.ubit = t_table_3.ubit - 2'b1;
                                    end
                                    else begin
                                        if (t_table_3.ubit == 2'b0) begin      //one u=0 then allocate entry
                                            t_table_3.ubit = 2'b0;
                                            t_table_3.tag = upd_pkt.comp_tag2_table[1];
                                            t_table_3.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                        else if (t_table_3.ubit != 2'b0 && t_table_2.ubit == 2'b0) begin
                                            t_table_2.ubit = 2'b0;
                                            t_table_2.tag = upd_pkt.comp_tag2_table[0];
                                            t_table_2.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011 ;
                                        end
                                    end
                                end
                    3'b011  :   begin
                                    if (t_table_3.ubit != 2'b0) begin
                                        t_table_3.ubit = t_table_3.ubit - 2'b1;
                                    end
                                    else begin
                                        t_table_3.ubit = 2'b0;
                                        t_table_3.tag = upd_pkt.comp_tag2_table[1];
                                        t_table_3.ctr = (upd_pkt.actual_outcome == 1'b1) ? 3'b100 : 3'b011;
                                    end
                                end
                endcase
            end

            bimodal.upd(ind0,t_bimodal);
            table_0.upd(ind1,t_table_0);
            table_1.upd(ind2,t_table_1);
            table_2.upd(ind3,t_table_2);
            table_3.upd(ind4,t_table_3);

            //$display("\nUpdation over");
        endmethod

        method Prediction_Packet output_packet();
            return pred_pkt;
        endmethod

    endmodule

endpackage
