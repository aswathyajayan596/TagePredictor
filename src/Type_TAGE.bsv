package Type_TAGE;

import Vector :: *;
import FShow :: *;

export Type_TAGE :: *;
`include "parameter.bsv"

typedef     1                     ACTUAL_outcome_bits;
typedef     2                     CTR_BIMODAL_BITS;
typedef     2                     U_bits;
typedef     3                     CTR_bits;
typedef     131                   GHRSIZE_bits;
typedef     3                     TABLENO_bits;
typedef     1                     ALTPRED_bits;
typedef     1                     PRED_bits;
typedef     64                    PC_bits;
typedef     1	                    MISPRED_bit;
typedef     32                    GEOMLENGTH_bits;
typedef     32                    TARGETLENGTH_bits;

typedef Bit#(PC_bits)             PC;                           //64bits
typedef Bit#(GHRSIZE_bits)        GHR;                          //131bits
typedef Bit#(TABLENO_bits)        TABLENO;                      //000, 001, 010, 011, 100
typedef Bit#(TLog#(`BIMODALSIZE)) BIMODALINDEX;                 //8bits
typedef Bit#(TLog#(`TABLESIZE))   INDEX;                        //7bits
typedef Bit#(CTR_BIMODAL_BITS)    CTR_BIMODAL;                  //2bits counter
typedef Bit#(CTR_bits)            CTR;                          //3bits counter
typedef Bit#(`TAG1_SIZE)	        TAG1;                         //8bits
typedef Bit#(`TAG2_SIZE)		      TAG2;                         //9bits
typedef Bit#(U_bits)              Usefulbits;                   //2bits
typedef Bit#(ACTUAL_outcome_bits) ACTUAL_OUTCOME;               //1bit
typedef Bit#(PRED_bits)           PRED;                         //1bit
typedef Bit#(ALTPRED_bits)        ALTPRED;                      //1bit
typedef Bit#(MISPRED_bit)	        MISPRED;                      //misprediction bit
typedef Bit#(GEOMLENGTH_bits)     GEOMETRIC;                    //geomlength of each table
typedef Bit#(TARGETLENGTH_bits)   TARGETLENGTH;                 //targetlength
typedef Bit#(`PHR_LEN)            PHR;

typedef struct {
    CTR ctr;
    TAG1 tag;
    Usefulbits ubit;
} Tag_Entry1 deriving(Bits, Eq, FShow);

typedef struct {
    CTR ctr;
    TAG2 tag;
    Usefulbits ubit;
} Tag_Entry2 deriving(Bits, Eq, FShow);

typedef struct {
	CTR_BIMODAL ctr;
} Bimodal_Entry deriving(Bits, Eq, FShow);



typedef struct {
    BIMODALINDEX                                  bimodalindex;
    Vector#(`NUMTAGTABLES, INDEX)                 index;
    Vector#(TSub#(`NUMTAGTABLES,2), TAG1)     	  comp_tag1_table;
    Vector#(TSub#(`NUMTAGTABLES,2), TAG2)     	  comp_tag2_table;
    Vector#(`NUMTAGTABLES, Usefulbits)  		      usefulbits;
    Vector#(TAdd#(`NUMTAGTABLES,1), CTR)          ctr;
    GHR             		                          ghr;
    PRED            		                          pred;
    TABLENO      	                                tableNo;
    ALTPRED         		                          altpred;
    PHR                                           phr;
   } Prediction_Packet deriving(Bits, Eq, FShow);

typedef struct {
    BIMODALINDEX	     		                        bimodalindex;
    Vector#(`NUMTAGTABLES, INDEX)                 index;
    Vector#(TSub#(`NUMTAGTABLES,2), TAG1)         comp_tag1_table;
    Vector#(TSub#(`NUMTAGTABLES,2), TAG2)         comp_tag2_table;
    Vector#(`NUMTAGTABLES, Usefulbits) 			      usefulbits;
    Vector#(TAdd#(`NUMTAGTABLES,1), CTR)          ctr;
    PRED            		                          pred;
    GHR             		                  	      ghr;
    TABLENO      	                                tableNo;
    ALTPRED         		                          altpred;
    MISPRED				                                mispred;
    ACTUAL_OUTCOME                                actual_outcome;
    PHR                                           phr;
    } Updation_Packet deriving(Bits,Eq, FShow);

typedef struct {
    Int#(32)                                      prediction_ctr;
    Int#(32)                                      misprediction_ctr;
} Table_ctrs deriving(Bits, Eq, FShow);

function Bit#(64) compHistFn(GHR ghr,TARGETLENGTH targetlength,GEOMETRIC geomlength);
    Bit#(32) mask = (1 << targetlength) - 32'b1;
    Bit#(32) mask1 = zeroExtend(ghr[geomlength]) << (geomlength % targetlength);
    Bit#(32) mask2 = (1 << targetlength);
    Bit#(32) compHist = 0;
    compHist = (compHist << 1) + zeroExtend(ghr[0]);
    compHist = compHist ^ ((compHist & mask2) >> targetlength);
    compHist = compHist ^ mask1;
    compHist = compHist & mask;
    return zeroExtend(compHist);
endfunction


//verilog code history function definition
function Bit#(64) compFoldIndex(PC pc,GHR ghr,PHR phr,TABLENO ti);

		Bit#(64) index = 0;
		if (ti == 3'b000) begin
		        index = pc[`BIMODAL_LEN - 1:0];        //13bit pc
			return index;
		end
		else if (ti == 3'b001) begin
      let comp_hist = compHistFn(ghr, `TABLE_LEN, `GEOMETRIC1);
			index = pc ^ (pc >> `TABLE_LEN) ^ comp_hist ^ zeroExtend(phr) ^ (zeroExtend(phr) >> `TABLE_LEN); // indexTagPred[0] = PC ^ (PC >> TAGPREDLOG) ^ indexComp[0].compHist ^ PHR ^ (PHR >> TAGPREDLOG);
	    return index;
		end
	  else if (ti == 3'b010) begin
			let comp_hist =  compHistFn(ghr, `TABLE_LEN, `GEOMETRIC2);
      index = pc ^ (pc >> (`TABLE_LEN - 1)) ^ comp_hist ^ zeroExtend(phr) ^ (zeroExtend(phr) >> `TABLE_LEN);
			return index;
		end
		else if (ti == 3'b011) begin
		  let comp_hist = compHistFn(ghr, `TABLE_LEN, `GEOMETRIC3);
      index = pc ^ (pc >> (`TABLE_LEN - 2)) ^ comp_hist ^ zeroExtend(phr) ^ (zeroExtend(phr) >> `TABLE_LEN);
			return index;
		end
		else begin
			let comp_hist = compHistFn(ghr, `TABLE_LEN, `GEOMETRIC4);
      index = pc ^ (pc >> (`TABLE_LEN) - 3) ^ comp_hist ^ zeroExtend(phr) ^ (zeroExtend(phr) >> `TABLE_LEN);
		  return index;
		end

endfunction

function Bit#(64) compFoldTag(PC pc, GHR ghr, TABLENO ti);
  Bit#(64) comp_tag_table = 0;
  if (ti == 3'b001) begin
    let comp_hist0 = compHistFn(ghr,`TAG2_SIZE, `GEOMETRIC1);
    let comp_hist1 = compHistFn(ghr,`TAG1_SIZE, `GEOMETRIC1);
    comp_tag_table = pc ^ comp_hist0 ^ (comp_hist1 << 1) ;
    return comp_tag_table;
    // tag[i] = PC ^ tagComp[0][i].compHist ^ (tagComp[1][i].compHist << 1);
  end
  else if (ti == 3'b010) begin
    let comp_hist0 = compHistFn(ghr,`TAG2_SIZE, `GEOMETRIC2);
    let comp_hist1 = compHistFn(ghr,`TAG1_SIZE, `GEOMETRIC2);
    comp_tag_table = pc ^ comp_hist0 ^ (comp_hist1 << 1) ;
    return comp_tag_table;
  end
  else if (ti == 3'b011) begin
    let comp_hist0 = compHistFn(ghr,`TAG2_SIZE, `GEOMETRIC3);
    let comp_hist1 = compHistFn(ghr,`TAG1_SIZE, `GEOMETRIC3);
    comp_tag_table = pc ^ comp_hist0 ^ (comp_hist1 << 1) ;
    return comp_tag_table;
  end
  else if (ti == 3'b100) begin
    let comp_hist0 = compHistFn(ghr,`TAG2_SIZE, `GEOMETRIC4);
    let comp_hist1 = compHistFn(ghr,`TAG1_SIZE, `GEOMETRIC4);
    comp_tag_table = pc ^ comp_hist0 ^ (comp_hist1 << 1) ;
    return comp_tag_table;
  end
  else
    return 0;
endfunction

endpackage
