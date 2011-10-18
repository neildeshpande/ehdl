/* An example of how we could describe a simple MIPS processor with 1 Branch Delay Slot! */
/* Notice that memories are not properly described. They must be replaced using a memory generator! */

/* 
   -Everything is parallel: it's hardware description.
   -Add the number of bits for representing the signal <int(#bit) id>
   -Possibility to split any variable using number of bits <id(7-0)>
   -Concatenation??
   -introducing a type bit for constants. For instance the opcode!
   -Multiple output allowed for functions through struct and type declaration
   -Initial value must be a constant. When specified,the initial value is used as reset value.
   -If not specified at reset they are not set. (std_logic 'X') --> useful for memories
   -ASYNC variables must not have an initial value!
   -Global variables can be written by one driver only!
*/

//OPCODE CONSTANTS - not exhaustive! (supports only immediate branches)
const unsigned int(6) rtype = 0;
const unsigned int(6) rtype2 = 28;
const unsigned int(6) loadb = 32;
const unsigned int(6) loadh = 33;
const unsigned int(6) loadw = 35;
const unsigned int(6) storeb = 40;
const unsigned int(6) storeh = 41;
const unsigned int(6) storew = 43;
const unsigned int(6) addi = 6;
const unsigned int(6) andi = 12;
const unsigned int(6) ori = 13;
const unsigned int(6) xori = 14;
const unsigned int(6) jump = 2;
const unsigned int(6) jumplink = 3;
const unsigned int(6) brancheq = 4;
const unsigned int(6) branchneq = 5;
const unsigned int(6) branchlez = 6;
const unsigned int(6) branchgtz = 7;

//FUNCT encoding for R-type instructions
const unsigned int(6) add = 32;
const unsigned int(6) sub = 34;
const unsigned int(6) and = 36;
const unsigned int(6) or = 37;
const unsigned int(6) xor = 38;
const unsigned int(6) slt = 43;
const unsigned int(6) jr = 8;

//ALU Function Control
const unsigned int(3) alu_add = 2;
const unsigned int(3) alu_sub = 6;
const unsigned int(3) alu_slt = 7;
const unsigned int(3) alu_or = 1;
const unsigned int(3) alu_xor = 4;
const unsigned int(3) alu_and = 0;


//ALU
int(32) inst_alu(int(32) a, int(32) b, int(32) imm,  int(1) imm_op, unsigned int(3) alu_fun){
  int(32) srca = a;
  int(32) srcb;
  int(32) res;
  if (imm_op) srcb = imm; else srcb=b;
  
  switch(alu_fun) {
  case alu_add : res = srca + srcb; break;
  case alu_sub : res = srca - srcb; break;
  case alu_slt : if (srca < src b) res = 1; else res = 0; break;
  case alu_or : res = srca | srcb; break; //logic OR
  case alu_xor : res = srca ^ srcb; break;
  case alu_and : res = srca && srcb; break;
  }
}

//Control Unit
struct cu_s {
  int(1) branch = 0;
  int(1) mem_rd = 0;
  int(1) mem_we = 0;
  unsigned int(2) mem_mask = 0;
  int(1) rf_we = 0;
  int(1) dest_rd = 0;
  unsigned int(3) alu_fun = 0;
  int(1) imm_op = 0;
  int(1) invalid_op = 0;
};

cu_s inst_cu(unsigned int(6) op, unsigned int (6) funct, int(1) comp_res){
  cu_s cu;

  switch(op) {
  case rtype : 
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 1;
    cu.imm_op = 0;
    switch(funct) {
    case add : cu.alu_fun = alu_add; invalid_op = 0; break;
    case sub : cu.alu_fun = alu_sub; invalid_op = 0; break;
    case and : cu.alu_fun = alu_and; invalid_op = 0; break;
    case or : cu.alu_fun = alu_or; invalid_op = 0; break;
    case xor : cu.alu_fun = alu_xor; invalid_op = 0; break;
    case slt : cu.alu_fun = alu_slt; invalid_op = 0; break;
    case jr : cu.alu_fun = alu_add; invalid_op = 0; cu.branch = 1; cu.rf_we = 0; break;
    default : cu.alu_fun = 0; invalid_op = 1;
    } break;
  case loadb :
    cu.branch = 0;
    cu.mem_rd = 1;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case loadh :
    cu.branch = 0;
    cu.mem_we = 0;
    cu.mem_rd = 1;
    cu.mem_mask = 1;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case loadw :
    cu.branch = 0;
    cu.mem_rd = 1;
    cu.mem_we = 0;
    cu.mem_mask = 3;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case storeb :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 1;
    cu.mem_mask = 0;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case storeh :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 1;
    cu.mem_mask = 1;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case storew :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 1;
    cu.mem_mask = 3;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case addi :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case andi :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_and;
    invalid_op = 0;
    break;
  case ori :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_or;
    invalid_op = 0;
    break;
  case xori :
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 1;
    cu.alu_fun = alu_xor;
    invalid_op = 0;
    break;
  case jump :
    cu.branch = 1;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 0;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case jumplink :
    cu.branch = 1;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 1;
    cu.dest_rd = 0;
    cu.imm_op = 0;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case brancheq | branchneq :
    cu.branch = comp_res;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 0;
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  case branchlez | branchgtz :
    cu.branch = comp_res;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 1; //set to 1 to have an easier condition for stalling!
    cu.alu_fun = alu_add;
    invalid_op = 0;
    break;
  default : 
    cu.branch = 0;
    cu.mem_rd = 0;
    cu.mem_we = 0;
    cu.mem_mask = 0;
    cu.rf_we = 0;
    cu.dest_rd = 0;
    cu.imm_op = 0;
    cu.alu_fun = alu_add;
    invalid_op = 1;
    break;
  }
  return cu;
}

int(32) imem[256]; //Instruction memory array not initialized
int(32) dmem[256]; //Data memory array not initialized

//instruction memory
int(32) inst_imem(unsigned int(32) adx){
  int(32) dout;

  dout = imem[adx];
  return dout;
}

//data memory
int(32) inst_dmem(unsigned int(32) adx, unsigned int(2) mask, int(32) din, int(1) we){

  if (we) {
    switch(mask) {
    case 0 : dmem[adx](7-0) = din(7-0); break;
    case 1 : dmem[adx](15-0) = din(15-0); break;
    case 2 : dmem[adx](23-0) = din(23-0); break;
    case 3 : dmem[adx]=din; break;
    }
    POS; //since the POS is inside the if, the register will have an enable.
    //a register is inferred for dmem
  }

  int(32) dout;
  switch (mask) {
  case 0 : dout = dmem[adx](7-0); break;  //automatic sign extension (not casting)         
  case 1 : dout = dmem[adx](15-0); break; //ram and data are signed int!
  case 2 : dout = dmem[adx](23-0); break;
  case 3 : dout = dmem[adx]; break;
      }
  return dout;
}


//register file
struct rfout_s {
  int(32) a = 0;
  int(1) va = 1; 
  int(32) b = 0;
  int(1) vb = 1;
};

int(32) rf[32] = 0;           //Register File initialized to 0
int(1) vrf[32] = 1;          //Every register is valed after the reset or deadlock! 

rfout_s rfout inst_rf(unsigned int(5) adxa, unsigned int(5) adxb, unsigned int(5) adxw, int(32) din, int(1) we, unsigned int(5) f_adxw, int(1) f_we, int(1) stall){
  rfout_s rfout;
  
  if(we && (adxw != 0)){
    rf[adxw] = din;
    vrf[adxw] = 1;
    POS;
  }

  if(f_we && !stall && (f_adxw != 0)){ //need to check stall or deadlock!
    vrf[f_adxw] = 0;
    POS;
  }

  rfout.a = rf[adxa];
  rfout.b = rf[adxb];
  rfout.va = vrf[adxa];
  rfout.vb = vrf[adxb];
  return rfout;
}


//Branch Target
unsigned int(32) inst_bt(unsigned int(32) npc, int(32) imm, unsigned int(26) j_index, int(32) a, unsigned int(6) op){
  unsigned int bt;
  int(32) imm_shift = imm << 2;
  int(28) j_index_shift = j_index << 2;

  switch (op){
  case jump | jumplink : bt(31-28) = npc(31-28); bt(27-0) = j_index_shift; break;
  case brancheq | branchneq | branchlez | branchgtz : bt = npc + imm_shift; break;
  case rtype : bt = a;
  default : bt = npc + imm_shift;
  }
  return bt;
}

//Branch comparator
int(1) inst_bc(int(32) a, int(32) b, unsigned int(6) op){
  //BLEZ and BGTZ compare rs with rt, but rt is always R0!
  int(1) comp_res, eq, lez, gtz, neq;
  if (a == b) eq = 1; else eq = 0;
  neq = !eq;
  if (a > 0) gtz = 1; else gtz =0;
  lez = !gtz;

  switch(op) {
  case brancheq : comp_res = eq; break;
  case branchneq : comp_res = neq; break;
  case branchlez : comp_res = lez; break;
  case branchgtz : comp_res = gtz; break;
  default : comp_res = 0;
  }
  return comp_res;
}


//Program Counter
unsigned int(32) inst_pc(unsigned int(32) npc, int(1) stall){
  unsigned int(32) pc = 0;      //Code-segment entry point

  if(!stall){
    pc = npc;
    POS;
  }
  return pc;
}


int(32)[] main(){

  //----------------------------------------FETCH STAGE-------------------------
  ASYNC int(1) stall, branch; //Assigned in ID
  ASYNC unsigned int(32) bt; //Assigned in ID
  unsigned int(32) npc = 0;  //At reset all pipeline registers must be zero
  unsigned int(32) pc = 0;
  unsigned int(32) ir = 0;

  if (branch)
    npc = bt;
  else
    npc = pc + 4;
    
  pc = inst_pc(npc, stall);
  ir = inst_imem(adx);

  // IF/ID register
  //Variables assigned are npc, pc, ir.
  //they are registered.
  if(!stall) POS;
  
  //--------------------------------------DECODE STAGE--------------------------

  unsigned int(6) op = 0;
  unsigned int(5) rs = 0;
  unsigned int(5) rt = 0;
  unsigned int(5) rd = 0;
  unsigned int(6) funct = 0;
  unsigned int(16) imm  = 0;
  unsigned int(31) imm_ex = 0;
  unsigned int(26) j_index = 0;
  ASYNC unsigned int(5) rf_dest; //assigned in WB
  ASYNC int(32) rf_din; //assinged in WB
  ASYNC int(1) rf_we; //assinged in WB
  int(32) a = 0;
  int(32) b = 0;
  int(1) va = 1;
  int(1) vb = 1;
  int(1) rd_a = 0;
  int(1) rd_b = 0;
  unsigned int(5) f_adxw = 0;
  int(1) f_we = 0;	

  op = ir(31-26);
  rs = ir(25-21);
  rt = ir(20-16);
  rd = ir(15-11);
  funct = ir(5-0);
  imm = ir(15-0);
  j_index = ir(25-0);
  imm_ex = imm; //Automatic sign extension

  rfout_s rfout; //initial value set inside the struct statement!
  int(1) comp_res;
  cu_s cu;

   /*Control Unit
struct cu_s {
  int(1) branch;
  int(1) mem_we;
  unsigned int(2) mem_mask;
  int(1) rf_we;
  int(1) dest_rd;
  unsigned int(3) alu_fun;
  int(1) imm_op;
  int(1) invalid_op;
  };*/
  cu = inst_cu(op, funct, comp_res);

  if ((op == rtype)|(op=brancheq)|(op=branchneq)){
    rd_a = 1;
    rd_b = 1;
  }
  else if (cu.imm_op) {
    rd_a = 1;
    rd_b = 0;
  }
  else {
    rd_a = 0;
    rd_b = 0;
  }

  if(cu.dest_rd) f_adxw = rd;
  else if (cu.branch) f_adxw = 31;
       else f_adxw = rt;

  f_we = cu.rf_we && !stall

  //register file
  /*struct rfout_s {
  int(32) a;
  int(1) va; 
  int(32) b;
  int(1) vb;
  };*/
  rfout = inst_rf(rs, rt, rf_dest, rf_din, rf_we, f_adxw, f_we, stall);
  a = rfout.a;
  b = rfout.b;
  va = rfout.va;
  vb = rfout.vb;

  if (rd_a && rd_b) stall = !va | !vb;
  else if rd_a stall = !va;
  else stall = 0;
  
  bt = inst_bt(npc, imm_ex, j_index, a, op);
  comp_res = inst_bc(a, b, op);
  
  branch = cu.branc;

  //ID/EX register
  if (!stall) POS;

  //-------------------------------EXECUTE STAGE---------------------------------
  int(32) alu_res;
  
  unsigned int pcp8 = pc + 8;
  alu_res = inst_alu(a, b, imm_ex, cu.imm_op, cu.alu_fun);

  //EX/MEM register
  POS;

  //-------------------------------MEMORY STAGE----------------------------------
  
  int(32) mem_dout;
  mem_dout = inst_dmem(alu_res, cu.mask, b, cu.mem_we);

  //MEM/WB register
  POS;

  //----------------------------WRITE BACK STAGE---------------------------------

  rf_we = cu.rf_we;
  rf_dest = f_adxw;
  
  if (cu.branch) rf_din = pcp8;
  else if (cu.mem_rd) rf_din = mem_dout;
       else rf_din = alu_res;

  return rf;
  
}
