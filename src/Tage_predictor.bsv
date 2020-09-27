package Tage_predictor;

    import Utils :: *;
    import Type_TAGE :: *;
    import RegFile :: *;
    import Vector :: *;

    `include "parameter.bsv"

    interface Tage_predictor_IFC;
        method Action computePrediction(ProgramCounter pc); //Indexing Table,Tag Computation, Comparison of Tag, Obtaining Prediction
        method Action updateTablePred(UpdationPacket upd_pkt);  //Updation of Usefulness Counter and Prediction Counter, Allocation of new entries in case of misprediction
        method PredictionPacket output_packet();    // Method to Output the prediction packet.
    endinterface

    function Vector#(4,TagEntry) allocate_entry(Vector#(4,TagEntry) entries, TableNo tno, Vector#(4,Tag) tags, ActualOutcome outcome);
            Bool allocate = False;
            for (Integer i = 3; i >= 0; i = i - 1) begin    
                if(entries[i].uCtr == 2'b0 && allocate == False) begin
                    entries[i].uCtr = 2'b0;
                    entries[i].tag = tags[i];
                    entries[i].ctr = (outcome == 1'b1) ? 3'b100 : 3'b011 ;
                    allocate = True;
                end
            end
            if (allocate == False) begin
                for (Integer i = 0; i < 4; i = i + 1) 
                entries[i].uCtr = 2'b0;
            end
            return entries;
    endfunction


    // function TagEntry allocate_entry(TagEntry entry, Tag tag, ActualOutcome outcome);
    //     entry.uCtr = 2'b0;
    //     entry.tag = tag;
    //     entry.ctr = (outcome == 1'b1) ? 3'b100 : 3'b011 ;
    //     return entry;
    // endfunction

    // function Tuple2#(Int#(3), Bool) entry_to_allocate(TagEntry t_table[], TableNo tno);
    //         Int#(3) tableNo = 0;
    //         Bool found = False;
    //         for (Int#(3) i = 3; i >= unpack(tno); i = i - 1) begin    
    //             if(t_table[i].uCtr == 2'b0 && found == False) begin
    //                 found = True;
    //                 tableNo = i;
    //             end
    //         end
    //         return tuple2(tableNo, found);
    // endfunction



    (*synthesize*)
    module mkTage_predictor(Tage_predictor_IFC);

        let bimodal_max = fromInteger(`BIMODALSIZE-1);   //maximum sixe for Regfile of Bimodal Predictor Table
        let table_max = fromInteger(`TABLESIZE-1);       //maximum size for RegFile of Predictor tables
        Reg#(GlobalHistory) ghr <- mkReg(0);            //internal register to store GHR
        Reg#(PredictionPacket) pred_pkt <- mkReg(unpack(0));  //output - index, tag1 & 2, usefulbits,ctr, ghr,prediction, tableNo, altpred initialised to 0
        RegFile#(BimodalIndex, BimodalEntry) bimodal <- mkRegFile(0, bimodal_max);
        RegFile#(TagTableIndex, TagEntry) table_0 <- mkRegFile(0, table_max);
        RegFile#(TagTableIndex, TagEntry) table_1 <- mkRegFile(0, table_max);
        RegFile#(TagTableIndex, TagEntry) table_2 <- mkRegFile(0, table_max);
        RegFile#(TagTableIndex, TagEntry) table_3 <- mkRegFile(0, table_max);
        Reg#(PathHistory) phr <- mkReg(0);
        RegFile#(TagTableIndex, TagEntry) tables[4] = {table_0, table_1, table_2, table_3};
        RWire#(UpdationPacket) rw_upd_pkt <- mkRWire();
        RWire#(Prediction)  rw_pred <- mkRWire();
        RWire#(Bit#(1)) upd_pkt_recvd <- mkRWire();
        Wire#(ProgramCounter) w_pc <- mkWire();

        rule rl_update_GHR;

            let t_ghr = ghr;
            let t_phr = phr;
            let updateRecvd = fromMaybe(0,upd_pkt_recvd.wget());    
            let t_u_pkt = fromMaybe (?, rw_upd_pkt.wget());

            if(updateRecvd == 1'b1 && t_u_pkt.mispred == 1'b1) begin // updation of GHR at updationPacket.
                t_u_pkt.ghr = (t_u_pkt.ghr >> 1);
                if(t_u_pkt.actualOutcome == 1)
                    t_ghr = (t_u_pkt.ghr << 1) + 131'b1;
                else
                    t_ghr = (t_u_pkt.ghr << 1);
                t_phr = (t_u_pkt.phr >> 1);
            end
            else if(updateRecvd == 1'b1 && t_u_pkt.mispred == 1'b0) begin
                t_ghr = t_u_pkt.ghr;
                if(t_u_pkt.actualOutcome == 1)
                    t_ghr = (t_u_pkt.ghr << 1) + 131'b1;
                else
                    t_ghr = (t_u_pkt.ghr << 1);
                t_phr = t_u_pkt.phr;
            end
            else begin                                             //speculative updation of GHR and PHR
                let pred = fromMaybe(?,rw_pred.wget());

                `ifdef DISPLAY 
                    $display("PC = %h", w_pc);
                `endif

                if(pred == 1'b1)
                    t_ghr = ( t_ghr  << 1 ) + 131'b1;
                else
                    t_ghr = ( t_ghr  << 1 );
                end

                t_phr = (t_phr << 1);
                if(w_pc[2] == 1'b1) begin
                    t_phr = t_phr + 32'b1;
                end

                `ifdef DISPLAY
                    $display("GHR after updation: %b",t_ghr);
                    $display("PHR after updation: %b",t_phr);
                `endif

                ghr <= t_ghr;
                phr <= t_phr;
        endrule


        method Action computePrediction(ProgramCounter pc);

            //tags
            Tag comp_tag[4];

            //indexes
            BimodalIndex bimodalindex;
            TagTableIndex index[4];

            //variable to store temporary prediction packet
            PredictionPacket t_pred_pkt = unpack(0);

            //updating PHR in temporary prediction packet
            t_pred_pkt.phr = phr;
            t_pred_pkt.phr = (t_pred_pkt.phr << 1);    //to append 0 to LSB
            if(pc[2] == 1'b1)
                t_pred_pkt.phr = t_pred_pkt.phr + 32'b1;   //to append 1 to LSB

            `ifdef DISPLAY
                $display("\nGHR before prediction = %h",ghr);
                $display("\n\nPrediction Packet of last Prediction\n",fshow(pred_pkt), cur_cycle);
                $display("Calculating Index..... ");
            `endif

            //calling index computation function for each table and calling tag computation function for each table
            bimodalindex = truncate(compFoldIndex(pc,ghr,t_pred_pkt.phr,3'b000));
            t_pred_pkt.bimodalindex = bimodalindex;
            for (Integer i = 0; i < 4; i=i+1) begin
                TableNo tNo = fromInteger(i+1);
                index[i] = truncate(compFoldIndex(pc,ghr,t_pred_pkt.phr,tNo));
                t_pred_pkt.tagTableindex[i] = index[i];
                if(i<2) begin
                    comp_tag[i] = tagged Tag2 truncate(compFoldTag(pc,ghr,tNo));
                    t_pred_pkt.tableTag[i] = comp_tag[i];
                end
                else begin
                    comp_tag[i] = tagged Tag1 truncate(compFoldTag(pc,ghr,tNo));
                    t_pred_pkt.tableTag[i] = comp_tag[i];
                end
            end


            //comparison of tag with the longest history table, getting prediction from it and alternate prediction from second longest tag matching table 
            t_pred_pkt.tableNo = 3'b000;
            t_pred_pkt.altpred = bimodal.sub(bimodalindex).ctr[1];
            t_pred_pkt.pred = bimodal.sub(bimodalindex).ctr[1];
            t_pred_pkt.ctr[0] = zeroExtend(bimodal.sub(bimodalindex).ctr);
            Bool matched = False;
            
            // for (Integer i = 3; i >= 0; i=i-1) begin
            //     if(tables[i].sub(index[i]).tag == comp_tag[i] && matched == False) begin
            //         if(matched) 
            //             t_pred_pkt.altpred = tables[i].sub(index[i]).ctr[2];
            //         else begin
            //             t_pred_pkt.ctr[i+1] = tables[i].sub(index[i]).ctr;
            //             t_pred_pkt.pred = tables[i].sub(index[i]).ctr[2];
            //             t_pred_pkt.tableNo = fromInteger(i+1);  
            //             t_pred_pkt.uCtr[i] = tables[i].sub(index[i]).uCtr; 
            //             matched = True;
            //         end
            //     end
            // end

            Bool altMatched = False;
            for (Integer i = 3; i >= 0; i=i-1) begin
                if(tables[i].sub(index[i]).tag == comp_tag[i] && !matched) begin
                        t_pred_pkt.ctr[i+1] = tables[i].sub(index[i]).ctr;
                        t_pred_pkt.pred = tables[i].sub(index[i]).ctr[2];
                        t_pred_pkt.tableNo = fromInteger(i+1); 
                        t_pred_pkt.uCtr[i] = tables[i].sub(index[i]).uCtr;        
                        matched = True;
                end
                else if(tables[i].sub(index[i]).tag == comp_tag[i] && matched && !altMatched) begin
                        t_pred_pkt.altpred = tables[i].sub(index[i]).ctr[2];
                        altMatched = True;
                end
            end

            

            t_pred_pkt.ghr = ghr;                       //storing current GHR in the temporary prediction packet
            rw_pred.wset(t_pred_pkt.pred);              //setting RWire for corresponding GHR updation in the rule
            w_pc<=pc;

            //speculative update of GHR storing in temporary prediction packet
            if(t_pred_pkt.pred == 1'b1)                 
                t_pred_pkt.ghr = ( t_pred_pkt.ghr  << 1 ) + 131'b1;
            else
                t_pred_pkt.ghr = ( t_pred_pkt.ghr  << 1 );

            pred_pkt <= t_pred_pkt;                     //assigning temporary prediction packet to prediction packet vector register
            `ifdef  DISPLAY
                $display("Current PC = %b", pc);
                $display("\nphr = %b",t_pred_pkt.phr);
                $display("\nPrediction Packet of current Prediction \n", fshow(t_pred_pkt), cur_cycle);
                $display("Prediction over....");
            `endif

        endmethod


        method Action updateTablePred(UpdationPacket upd_pkt);  //

            rw_upd_pkt.wset(upd_pkt);
            upd_pkt_recvd.wset(1'b1);

            //store the indexes of each entry of predictor tables from the updation packet
            //Store the corresponding indexed entry whose index is obtained from the updation packet
            TagTableIndex ind[4];
            Vector#(4,TagEntry) t_table;
            Vector#(4,Tag) table_tags;

            BimodalIndex bindex = upd_pkt.bimodalindex;
            BimodalEntry t_bimodal = bimodal.sub(bindex);
            for(Integer i=0; i < 4; i=i+1) begin
                ind[i] = upd_pkt.tagTableindex[i];
                t_table[i] = tables[i].sub(ind[i]);
                table_tags[i] = upd_pkt.tableTag[i];
            end

            //store the actual outcome from the updation packet
            ActualOutcome outcome = upd_pkt.actualOutcome;

            `ifdef DISPLAY
            $display("\n\nUpdation Packet\n",fshow(upd_pkt));
            $display("Updation Packet Table Number = %b",upd_pkt.tableNo);
            $display("GHR = %h", upd_pkt.ghr );
            `endif

            //Updation of usefulness counter
            /* Usefulness counter is updated if the final prediction is different from alternate prediction, u is incremented if the prediction is correct
            u is decremented otherwise */

            if(upd_pkt.pred != upd_pkt.altpred) begin
            if (upd_pkt.mispred == 1'b0)
            t_table[upd_pkt.tableNo-1].uCtr = t_table[upd_pkt.tableNo-1].uCtr + 2'b1;
            else
            t_table[upd_pkt.tableNo-1].uCtr = t_table[upd_pkt.tableNo-1].uCtr - 2'b1;
            end


            // updation of provider component's prediction counter
            /* Provider component's prediction counter is incremented if actual outcome is TAKEN and decremented if actual outcome is NOT TAKEN */
            if(upd_pkt.actualOutcome == 1'b1) begin
                if(upd_pkt.tableNo == 3'b000)
                    t_bimodal.ctr = (t_bimodal.ctr < 2'b11) ? (t_bimodal.ctr + 2'b1) : 2'b11;
                else
                    t_table[upd_pkt.tableNo-1].ctr = (t_table[upd_pkt.tableNo-1].ctr < 3'b111 )?(t_table[upd_pkt.tableNo-1].ctr + 3'b1): 3'b111;
            end
            else begin
                if(upd_pkt.tableNo == 3'b000)
                    t_bimodal.ctr = (t_bimodal.ctr > 2'b00) ? (t_bimodal.ctr - 2'b1) : 2'b00;
                else
                    t_table[upd_pkt.tableNo-1].ctr = (t_table[upd_pkt.tableNo-1].ctr > 3'b000)?(t_table[upd_pkt.tableNo-1].ctr - 3'b1): 3'b000;
            end

            //Allocation of new entries if there is a misprediction
            /* Allocate new entry, if there is any u = 0 (not useful entry) for tables with longer history 
            Three cases arise: all u>0 , one u = 0, more than one u = 0
            For all u > 0, decrement all the u counters, No need to allocate new entry
            For one u = 0, allocate new entry to that index
            For more than one u = 0, allocate new entry to that which has longer history
            For the newly allocated entry, prediction counter is set to Weakly TAKEN or Weakly NOT TAKEN.
            For the newly allocated entry, usefuleness counter is set to 0.
            For the newly allocated entry, tag is computed tag stored in the updation packet for that entry
            */

            
            
            // if (upd_pkt.mispred == 1'b1) begin
            //     Bool allocated = False;
            //     case (upd_pkt.tableNo)
            //         3'b000 :    begin
            //                         Tuple2#(Int#(3), Bool) allocation = entry_to_allocate(t_table, 3'b000);
            //                         match {.tno, .found} = allocation;
            //                         if (found) begin
            //                             t_table[tno] = allocate_entry(t_table[tno], upd_pkt.tableTag[tno], upd_pkt.actualOutcome); 
            //                             allocated = True;
            //                         end
            //                     end
            //         3'b001 :    begin
            //                         Tuple2#(Int#(3), Bool) allocation = entry_to_allocate(t_table, 3'b001);
            //                         match {.tno, .found} = allocation;
            //                         if (found) begin
            //                             t_table[tno] = allocate_entry(t_table[tno], upd_pkt.tableTag[tno], upd_pkt.actualOutcome); 
            //                             allocated = True;
            //                         end
            //                     end
            //         3'b010 :    begin
            //                         Tuple2#(Int#(3), Bool) allocation = entry_to_allocate(t_table, 3'b010);
            //                         match {.tno, .found} = allocation;
            //                         if (found) begin
            //                             t_table[tno] = allocate_entry(t_table[tno], upd_pkt.tableTag[tno], upd_pkt.actualOutcome); 
            //                             allocated = True;
            //                         end
            //                     end
            //         3'b011 :    begin
            //                         Tuple2#(Int#(3), Bool) allocation = entry_to_allocate(t_table, 3'b011);
            //                         match {.tno, .found} = allocation;
            //                         if (found) begin
            //                             t_table[tno] = allocate_entry(t_table[tno], upd_pkt.tableTag[tno], upd_pkt.actualOutcome); 
            //                             allocated = True;
            //                         end
            //                     end
            //     endcase
            //     if(!allocated) begin
            //         for (Integer i = 0; i < 4; i = i + 1) 
            //                 t_table[i].uCtr = 2'b0;
            //     end
                      
            // end
            

            if (upd_pkt.mispred == 1'b1) begin
                case (upd_pkt.tableNo)
                    3'b000 :    t_table = allocate_entry(t_table, 3'b000, table_tags, upd_pkt.actualOutcome);
                    3'b001 :    t_table = allocate_entry(t_table, 3'b001, table_tags, upd_pkt.actualOutcome);
                    3'b010 :    t_table = allocate_entry(t_table, 3'b010, table_tags, upd_pkt.actualOutcome);
                    3'b011 :    t_table = allocate_entry(t_table, 3'b011, table_tags, upd_pkt.actualOutcome);
                endcase
            end                    
            

            // if (upd_pkt.mispred == 1'b1) begin
            //     case (upd_pkt.tableNo)
            //         3'b000 :    
            //             begin
            //                 Bool allocate = False;
            //                 for (Integer i = 3; i >= 0; i = i - 1) begin    
            //                     if(t_table[i].uCtr == 2'b0 && allocate == False) begin
            //                         t_table[i].uCtr = 2'b0;
            //                         t_table[i].tag = upd_pkt.tableTag[i];
            //                         t_table[i].ctr = (upd_pkt.actualOutcome == 1'b1) ? 3'b100 : 3'b011 ;
            //                         allocate = True;
            //                     end
            //                 end
            //                 if (allocate == False) begin
            //                     for (Integer i = 0; i < 4; i = i + 1) 
            //                     t_table[i].uCtr = 2'b0;
            //                 end
            //             end
            //         3'b001 :   
            //             begin
            //                 Bool allocate = False;
            //                 for (Integer i = 3; i >= 1; i = i - 1) begin
            //                     if(t_table[i].uCtr == 2'b0 && allocate == False) begin
            //                     t_table[i].uCtr = 2'b0;
            //                     t_table[i].tag = upd_pkt.tableTag[i];
            //                     t_table[i].ctr = (upd_pkt.actualOutcome == 1'b1) ? 3'b100 : 3'b011 ;
            //                     allocate = True;
            //                     end
            //                 end
            //                 if (allocate == False) begin
            //                     for (Integer i = 1; i < 4; i = i + 1) 
            //                     t_table[i].uCtr = 2'b0;
            //                 end
            //             end
            //         3'b010 :    
            //             begin
            //                 Bool allocate = False;
            //                 for (Integer i = 3; i >= 2; i = i - 1) begin
            //                     if(t_table[i].uCtr == 2'b0 && allocate == False) begin
            //                         t_table[i].uCtr = 2'b0;
            //                         t_table[i].tag = upd_pkt.tableTag[i];
            //                         t_table[i].ctr = (upd_pkt.actualOutcome == 1'b1) ? 3'b100 : 3'b011 ;
            //                         allocate = True;
            //                     end
            //                 end
            //                 if (allocate == False) begin
            //                     for (Integer i = 2; i < 4; i = i + 1) 
            //                         t_table[i].uCtr = 2'b0;
            //                 end
            //             end
            //         3'b011 :    
            //             begin 
            //                 Bool allocate = False;
            //                 if(t_table[3].uCtr == 2'b0 && allocate == False) begin
            //                     t_table[3].uCtr = 2'b0;
            //                     t_table[3].tag = upd_pkt.tableTag[3];
            //                     t_table[3].ctr = (upd_pkt.actualOutcome == 1'b1) ? 3'b100 : 3'b011 ;
            //                     allocate = True;
            //                 end
            //                 if (allocate == False) begin
            //                     t_table[3].uCtr = 2'b0;
            //                 end
            //             end
            //     endcase
            // end

            //Assigning back the corresponding entries to the prediction tables.
            bimodal.upd(bindex,t_bimodal);
            for(Integer i = 0 ; i < 4; i = i+1)
                tables[i].upd(ind[i], t_table[i]);

            `ifdef DISPLAY
                $display("\nUpdation over");
            `endif

        endmethod

        method PredictionPacket output_packet(); //method that outputs the prediction packet
            return pred_pkt;
        endmethod

    endmodule

endpackage
