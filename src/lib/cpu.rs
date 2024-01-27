
use std::thread;
use core::time::Duration;
use crate::opcodes;
use crate::memory::Memory;
use crate::registers::Register;
use crate::registers::Flag;
use crate::opcodes::Opcode;

pub static mut REG : Register = Register{
    af: 0x0000,
    bc: 0x0000,
    de: 0x0000,
    hl: 0x0000,
    af_p: 0x0000,
    bc_p: 0x0000,
    de_p: 0x0000,
    hl_p: 0x0000,
    ix: 0x0000,
    iy: 0x0000,
    sp: 0x0000,
    pc: 0x0000,
    ir: 0x0000,
    wz: 0x0000
  };

fn read_reg8(reg : &str) -> u8 {
  unsafe {
    REG.read8(reg)
  }
}

fn read_reg16(reg : &str) -> u16 {
  unsafe {
    REG.read16(reg)
  }
}

fn write_reg8(reg : &str, val: u8) {
  unsafe {
    REG.write8(reg, val);
  }
}

fn write_reg16(reg : &str, val: u16) {
  unsafe {
    REG.write16(reg, val);
  }
}

fn set_flag(flag: Flag) {
  unsafe {
    REG.set_flag(flag);
  }
}

fn clear_flag(flag: Flag) {
  unsafe {
    REG.clear_flag(flag);
  }
}

fn is_flag_set(flag: Flag) -> bool {
  unsafe {
    REG.is_flag_set(flag)
  }
}

fn report_unknown(opcode_str: &str) {
  println!("Unknown {} instruction!", opcode_str);
}

fn report_not_supported(opcode_str: &str) {
  println!("{} instruction is not supported on the z80 - z180 only!", opcode_str);
}

pub trait InstTrait {
  // z80 instruction set
  fn nop(&self);
  fn ld(&self);
  fn ex(&self);
  fn exx(&self);
  fn inc(&self);
  fn dec(&self);
  fn push(&self);
  fn pop(&self);
  fn add(&self);
  fn adc(&self);
  fn sub(&self);
  fn sbc(&self);
  fn neg(&self);
  fn mlt(&self);
  fn and(&self);
  fn or(&self);
  fn xor(&self);
  fn cp(&self);
  fn daa(&self);
  fn cpl(&self);
  fn scf(&self);
  fn ccf(&self);
  fn rla(&self);
  fn rra(&self);
  fn rlca(&self);
  fn rrca(&self);
  fn rrc(&self);
  fn rlc(&self);
  fn rr(&self);
  fn rl(&self);
  fn rrd(&self);
  fn rld(&self);
  fn sla(&self);
  fn sra(&self);
  fn sll(&self);
  fn srl(&self);
  fn bit(&self);
  fn res(&self);
  fn set(&self);
  fn jp(&self);
  fn jr(&self);
  fn call(&self);
  fn djnz(&self);
  fn ret(&self);
  fn reti(&self);
  fn retn(&self);
  fn rst(&self);
  fn ldd(&self);
  fn ldir(&self);
  fn lddr(&self);
  fn cpir(&self);
  fn cpdr(&self);
  fn otir(&self);
  fn otdr(&self);
  fn inir(&self);
  fn indr(&self);
  fn _in(&self);
  fn ini(&self);
  fn ind(&self);
  fn out(&self);
  fn outi(&self);
  fn outd(&self);
  fn im(&self);
  fn di(&self);
  fn ei(&self);
  fn halt(&self);
  fn ldi(&self);
  fn cpi(&self);
  fn cpd(&self);
  // z180 only instruction set
  // (Not currently implemented)
  fn in0(&self);
  fn out0(&self);
  fn tst(&self);
  fn tstio(&self);
  fn slp(&self);
  fn otim(&self);
  fn otdm(&self);
  fn otimr(&self);
  fn otdmr(&self);
}

pub struct Instruction {
  code: u8,
  data: u16,
  len: usize,
  table: String,
  cycles: (u8,u8),
  sel_cycles: u8
}

impl InstTrait for Instruction {

  // NOP instruction
  fn nop(&self) {
    if self.code != 0x00 {
      report_unknown("NOP");
    }
  }

  // LD instruction
  fn ld(&self) {
    // ToDo: LD for IX & IY
    // Do actual instructions and set flags etc.
    if self.table == "main" || self.table == "misc" {
      if self.len == 0 {
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        if self.code & 0xC0 == 0x40 {
          // LD r, r
          let src = (self.code & 0x07) as usize;
          let dst = ((self.code >> 3) & 0x07) as usize;
          let src_reg = reg_map[src];
          let dst_reg = reg_map[dst];
          if src_reg == dst_reg {
            return; // pointless instruction
          }
          else if src_reg == "(HL)" {
            let hl : u16 = read_reg16("HL");
            let val = Memory::read8(hl);
            write_reg8(dst_reg, val);
          }
          else if dst_reg == "(HL)" {
            let val = read_reg8(src_reg);
            let hl : u16 = read_reg16("HL");
            Memory::write8(hl, val);
          }
          else {
            let val = read_reg8(src_reg);
            write_reg8(dst_reg, val);
          }
        }
        else if self.code == 0x02 {
          // LD (BC), A
          let val = read_reg8("A");
          let bc : u16 = read_reg16("BC");
          Memory::write8(bc, val);
        }
        else if self.code == 0x12 {
          // LD (DE), A
          let val = read_reg8("A");
          let de : u16 = read_reg16("DE");
          Memory::write8(de, val);
        }
        else if self.code == 0x0A {
          // LD A, (BC)
          let bc : u16 = read_reg16("BC");
          let val = Memory::read8(bc);
          write_reg8("A", val);
        }
        else if self.code == 0x1A {
          // LD A, (DC)
          let de : u16 = read_reg16("DE");
          let val = Memory::read8(de);
          write_reg8("A", val);
        }
        else if self.code == 0xF9 {
          // LD SP, HL
          let hl : u16 = read_reg16("HL");
          write_reg16("SP", hl);
        }
        else {
          report_unknown("LD");
        }
      }
      else if self.len == 1 {
        // LD r, n
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        if self.code & 0xC7 == 0x06 {
          let val = self.data as u8;
          let dst = ((self.code & 0x38) >> 3) as usize;
          let dst_reg = reg_map[dst];
          write_reg8(dst_reg, val);
        }
        else {
          report_unknown("LD");
        }
      }
      else if self.len == 2 {
        let reg_map = ["BC", "DE", "HL", "SP"];
        if self.code & 0xCF == 0x01 {
          // LD rr, nn
          let val = self.data;
          let dst = ((self.code & 0x30) >> 4) as usize;
          let dst_reg = reg_map[dst];
          write_reg16(dst_reg, val);
        }
        else if self.code & 0xCF == 0x43 {
          // LD (nn), rr
          let dst_addr = self.data;
          let src = ((self.code & 0x30) >> 4) as usize;
          let src_reg = reg_map[src];
          let val = read_reg16(src_reg);
          Memory::write16(dst_addr, val);
        }
        else if self.code & 0xCF == 0x4B {
          // LD rr, (nn)
          let src_addr = self.data;
          let dst = ((self.code & 0x30) >> 4) as usize;
          let dst_reg = reg_map[dst];
          let val = Memory::read16(src_addr);
          write_reg16(dst_reg, val);
        }
        else if self.code == 0x22 {
          // LD (nn), HL
          let dst_addr = self.data;
          let val = read_reg16("HL");
          Memory::write16(dst_addr, val);
        }
        else if self.code == 0x2A {
          // LD HL, (nn)
          let src_addr = self.data;
          let val = Memory::read16(src_addr);
          write_reg16("HL", val);
        }
        else if self.code == 0x32 {
          // LD (nn), A
          let dst_addr = self.data;
          let val = read_reg8("A");
          Memory::write8(dst_addr, val);
        }
        else if self.code == 0x3A {
          // LD A, (nn)
          let src_addr = self.data;
          let val = Memory::read8(src_addr);
          write_reg8("A", val);
        }
        else {
          report_unknown("LD");
        }
      }
    }
    else {
      // Todo: add IX and IY instructions here
      report_unknown("LD");
    }
  }

  fn ex(&self) {
    if self.table == "main" {
      if self.code == 0xEB {
        // EX DE, HL
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        write_reg16("HL", de);
        write_reg16("DE", hl);
      }
      else if self.code == 0x08 {
        // EX AF, AF'
        let af = read_reg16("AF");
        let af_p = read_reg16("AF'");
        write_reg16("AF'", af);
        write_reg16("AF", af_p);
      }
      else if self.code == 0xE3 {
        // EX (SP), HL
        let sp = read_reg16("SP");
        let hl = read_reg16("HL");
        let data = Memory::read16(sp);
        write_reg16("HL", data);
        Memory::write16(sp, hl);
      }
      else {
        // ToDo: IX and IY
        report_unknown("EX");
      }
    }
    else {
      report_unknown("EX");
    }
  }

  fn exx(&self) {
    if self.table == "main" {
      if self.code == 0xD9 {
        // EXX
        let bc = read_reg16("BC");
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        let bc_p = read_reg16("BC'");
        let de_p = read_reg16("DE'");
        let hl_p = read_reg16("HL'");
        write_reg16("BC", bc_p);
        write_reg16("DE", de_p);
        write_reg16("HL", hl_p);
        write_reg16("BC'", bc);
        write_reg16("DE'", de);
        write_reg16("HL'", hl);
      }
      else {
        report_unknown("EXX");
      }
    }
    else {
      report_unknown("EXX");
    }
  }

  // INC instruction
  #[allow(unused_assignments)]
  fn inc(&self) {
    // ToDo: IX and IY
    if self.table == "main" {
      if self.code & 0xC7 == 0x04 {
        // INC r
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        let dst = ((self.code & 0x38) >> 3) as usize;
        let reg = reg_map[dst];
        let mut addr : u16 = 0x0000;
        let mut val : u8 = 0x00;
        if reg == "(HL)" {
          addr = read_reg16("HL");
          val = Memory::read8(addr);
        }
        else  {
          val = read_reg8(reg);
        }
        let inc_val = (((val as u16) + 1) & 0xFF) as u8;
        if val == 0x7F {
          set_flag(Flag::PV);
        }
        else {
          clear_flag(Flag::PV);
        }
        if inc_val == 0x00 {
          set_flag(Flag::Z);
        }
        else {
          clear_flag(Flag::Z);
        }
        if val & 0x0F == 0x0F {
          set_flag(Flag::H);
        }
        else {
          clear_flag(Flag::H);
        }
        if inc_val & 0x80 == 0x80  {
          set_flag(Flag::S);
        }
        else {
          clear_flag(Flag::S);
        }
        clear_flag(Flag::N);
        if reg == "(HL)" {
          Memory::write8(addr, inc_val);
        }
        else {
          write_reg8(reg, inc_val);
        }
      }
      else if self.code & 0xCF == 0x03 {
        // INC rr
        let reg_map = ["BC", "DE", "HL", "SP"];
        let dst = ((self.code & 0x30) >> 4) as usize;
        let reg = reg_map[dst];
        let val = read_reg16(reg);
        if val == 0xFFFF {
          write_reg16(reg, 0);
        }
        else {
          write_reg16(reg, val + 1);
        }
      }
      else {
        report_unknown("INC");
      }
    }
    else {
      report_unknown("INC");
    }
  }

  // DEC instruction
  #[allow(unused_assignments)]
  fn dec(&self) {
    // ToDo: IX and IY
    if self.table == "main" {
      if self.code & 0xC7 == 0x05 {
        // DEC r
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        let dst = ((self.code & 0x38) >> 3) as usize;
        let reg = reg_map[dst];
        let mut addr : u16 = 0x0000;
        let mut val : u8 = 0x00;
        if reg == "(HL)" {
          addr = read_reg16("HL");
          val = Memory::read8(addr);
        }
        else  {
          val = read_reg8(reg);
        }
        let mut dec_val : u8 = 0x00;
        if val == 0x00 {
          dec_val = 0xFF;
        }
        else {
          dec_val = val-1;
        }
        if val == 0x80 {
          set_flag(Flag::PV);
        }
        else {
          clear_flag(Flag::PV);
        }
        if dec_val == 0x00 {
          set_flag(Flag::Z);
        }
        else {
          clear_flag(Flag::Z);
        }
        if val & 0x1F == 0x10 {
          set_flag(Flag::H);
        }
        else {
          clear_flag(Flag::H);
        }
        if dec_val & 0x80 == 0x80  {
          set_flag(Flag::S);
        }
        else {
          clear_flag(Flag::S);
        }
        clear_flag(Flag::N);
        if reg == "(HL)" {
          Memory::write8(addr, dec_val);
        }
        else {
          write_reg8(reg, dec_val);
        }
      }
      else if self.code & 0xCF == 0x0B {
        // DEC rr
        let reg_map = ["BC", "DE", "HL", "SP"];
        let dst = ((self.code & 0x30) >> 4) as usize;
        let reg = reg_map[dst];
        let val = read_reg16(reg);
        if val == 0x0000 {
          write_reg16(reg, 0xFFFF);
        }
        else {
          write_reg16(reg, val - 1);
        }
      }
      else {
        report_unknown("DEC");
      }
    }
    else {
      report_unknown("DEC");
    }
  }

  fn push(&self) {
    if self.code & 0xCF == 0xC5 { 
      let reg_map = ["BC", "DE", "HL", "SP"];
      let src = ((self.code & 0x30) >> 4) as usize;
      let src_reg = reg_map[src];
      let val = read_reg16(src_reg);
      let mut addr = read_reg16("SP");
      if addr == 0x0000 {
        addr = 0xFFFE;
      }
      else {
        addr = addr - 2;
      }
      write_reg16("SP", addr);
      Memory::write16(addr, val);
    }
    // todo: handle ix & iy
    else {
      report_unknown("PUSH");
    }
  }

  fn pop(&self) {
    if self.code & 0xCF == 0xC1 {
      let reg_map = ["BC", "DE", "HL", "SP"];
      let dst = ((self.code & 0x30) >> 4) as usize;
      let dst_reg = reg_map[dst];
      let mut addr = read_reg16("SP");
      let val = Memory::read16(addr);
      write_reg16(dst_reg, val);
      if addr == 0xFFFE {
        addr = 0x0000;
      }
      else {
        addr = addr + 2;
      }
      write_reg16("SP", addr);
    }
    // todo: handle ix & iy
    else {
      report_unknown("POP");
    }
  }

  fn add(&self) {
    report_unknown("ADD");
  }

  fn adc(&self) {
    report_unknown("ADC");
  }

  fn sub(&self) {
    report_unknown("SUB");
  }

  fn sbc(&self) {
    report_unknown("SBC");
  }

  fn and(&self) {
   report_unknown("AND");
  }

  fn or(&self) {
   report_unknown("OR");
  }

  fn xor(&self) {
   report_unknown("XOR");
  }

  fn cp(&self) {
    report_unknown("CP");
  }

  fn daa(&self) {
    report_unknown("DAA");
  }

  fn cpl(&self) {
    report_unknown("CPL");
  }

  fn scf(&self) {
    report_unknown("SCF");
  }

  fn ccf(&self) {
    report_unknown("CCF");
  }

  fn rla(&self) {
   report_unknown("RLA");
  }

  fn rra(&self) {
   report_unknown("RRA");
  }

  fn rlca(&self) {
   report_unknown("RLCA");
  }

  fn rrca(&self) {
   report_unknown("RRCA");
  }

  fn rrc(&self) {
   report_unknown("RRC");
  }

  fn rlc(&self) {
   report_unknown("RLC");
  }

  fn rr(&self) {
   report_unknown("RR");
  }

  fn rl(&self) {
   report_unknown("RL");
  }

  fn sla(&self) {
   report_unknown("SLA");
  }

  fn sra(&self) {
   report_unknown("SRA");
  }

  fn sll(&self) {
   report_unknown("SLL");
  }

  fn srl(&self) {
   report_unknown("SRL");
  }

  fn bit(&self) {
    report_unknown("BIT");
  }

  fn res(&self) {
    report_unknown("RES");
  }

  fn set(&self) {
    report_unknown("SET");
  }

  fn jp(&self) {
    if self.table == "main" {
      if self.code == 0xC3 {
        // JP nn
        let dst_addr = self.data;
        write_reg16("PC", dst_addr);
      }
      else if self.code == 0xE9 {
        // JP (HL)
        let hl = read_reg16("HL");
        let dst_addr = Memory::read16(hl);
        write_reg16("PC", dst_addr);
      }
      else if self.code & 0xC7 == 0xC2 {
        // JP cc,nn
        let dst_addr = self.data;
        let cc = (self.code & 0x38) >> 3;
        let cc_map = ["NZ", "Z", "NC", "C", "PO", "PE", "P", "M"];
        let cc_str = cc_map[cc as usize];
        let should_jump = match cc_str {
          "NZ" => { !is_flag_set(Flag::Z) },
          "Z" => { is_flag_set(Flag::Z) },
          "NC" => { !is_flag_set(Flag::C) },
          "C" => { !is_flag_set(Flag::C) },
          "PO" => { is_flag_set(Flag::PV) },
          "PE" => { !is_flag_set(Flag::PV) },
          "P" => { is_flag_set(Flag::S) },
          "M" => { !is_flag_set(Flag::S) },
          _ => { 
            println!("Unknown condition code for JP instruction!");
            false
         }
        };
        if should_jump {
          write_reg16("PC", dst_addr);
        }
      }
      else if self.table == "ix" {
        if self.code == 0xE9 {
          // JP (IX)
          let hl = read_reg16("IX");
          let dst_addr = Memory::read16(hl);
          write_reg16("PC", dst_addr);
        }
      }
      else if self.table == "iy" {
        if self.code == 0xE9 {
          // JP (IY)
          let hl = read_reg16("IY");
          let dst_addr = Memory::read16(hl);
          write_reg16("PC", dst_addr);
        }
      }
    }
    else {
      report_unknown("JP");
    }
  }

  fn jr(&self) {
    report_unknown("JR");
  }

  fn call(&self) {
    report_unknown("CALL");
  }

  fn djnz(&self) {
    report_unknown("DJNZ");
  }

  fn ret(&self) {
    report_unknown("RET");
  }

  fn reti(&self) {
    report_unknown("RETI");
  }

  fn retn(&self) {
    report_unknown("RETN");
  }

  fn rst(&self) {
    report_unknown("RST");
  }

  fn ldir(&self) {
    report_unknown("IDIR");
  }

  fn lddr(&self) {
    report_unknown("IDDR");
  }

  fn cpir(&self) {
    report_unknown("CPIR");
  }

  fn cpdr(&self) {
    report_unknown("CPDR");
  }

  fn otir(&self) {
    report_unknown("OTIR");
  }

  fn otdr(&self) {
    report_unknown("OTDR");
  }

  fn _in(&self) {
    report_unknown("IN");
  }

  fn indr(&self) {
    report_unknown("INR");
  }

  fn ini(&self) {
    report_unknown("INI");
  }

  fn inir(&self) {
    report_unknown("INIR");
  }

  fn ind(&self) {
    report_unknown("IND");
  }

  fn out(&self) {
    report_unknown("OUT");
  }

  fn di(&self) {
    report_unknown("DI");
  }

  fn ei(&self) {
    report_unknown("EI");
  }

  fn neg(&self) {
    report_unknown("NEG");
  }

  fn mlt(&self) {
    report_unknown("MLT");
  }

  fn rrd(&self) {
    report_unknown("RRD");
  }

  fn rld(&self) {
    report_unknown("RLD");
  }

  fn ldd(&self) {
    report_unknown("LDD");
  }

  fn outi(&self) {
    report_unknown("OUTI");
  }

  fn outd(&self) {
    report_unknown("OUTD");
  }

  fn im(&self) {
    report_unknown("IM");
  }

  fn halt(&self) {
    report_unknown("HALT");
  }

  fn ldi(&self) {
    report_unknown("LDI");
  }

  fn cpi(&self) {
    report_unknown("CPI");
  }

  fn cpd(&self) {
    report_unknown("CPD");
  }

  fn in0(&self) {
    report_not_supported("IN0");
  }

  fn out0(&self) {
    report_not_supported("OUT0");
  }

  fn tst(&self) {
    report_not_supported("TST");
  }

  fn tstio(&self) {
    report_not_supported("TSTIO");
  }

  fn slp(&self) {
    report_not_supported("SLP");
  }

  fn otim(&self) {
    report_not_supported("OTIM");
  }

  fn otdm(&self) {
    report_not_supported("OTDM");
  }

  fn otimr(&self) {
    report_not_supported("OTIMR");
  }

  fn otdmr(&self) {
    report_not_supported("OTDMR");
  }

}

pub struct Cpu;

impl Cpu {

  pub fn set_pc(addr: u16) {
    write_reg16("PC", addr);
  }

  pub fn get_pc() -> u16 {
    read_reg16("PC")
  }

  pub fn set_register16(reg: &str, addr: u16) {
    write_reg16(reg, addr);
  }

  pub fn get_register16(reg: &str) -> u16 {
    read_reg16(reg)
  }

  pub fn disassemble(addr: u16) -> (String, u8) {
    let (opcode, data, n) = Self::fetch(addr);
    let inst = opcode.instruction;
    if n == 2 {
        let s = format!("{:02X?}{:02X?}H", data[1], data[0]);
        (inst.replace("nn", s.as_str()), opcode.byte_count)
    }
    else if n == 1 {
        let s = format!("{:02X?}H", data[0]);
        if inst.contains("n") {
            (inst.replace("n", s.as_str()), opcode.byte_count)
        }
        else if inst.contains("d") {
            (inst.replace("d", s.as_str()), opcode.byte_count)
        }
        else {
            (String::from(inst), opcode.byte_count)
        }
    }
    else {
        (String::from(inst), opcode.byte_count)
    }
  }

  fn fetch(mut address: u16) -> (&'static Opcode<'static>, [u8;2], usize)
  {
      let code = Memory::read8(address);
      let mut opcode = &opcodes::MAIN_OPCODES[code as usize];
      address = (((address  as u32) + 1) & 0xFFFF) as u16;
      let mut inst_len : usize = 1;
      if opcode.instruction == "MISC" {
        let code = Memory::read8(address);
        opcode = &opcodes::MISC_OPCODES[code as usize];
        address = (((address  as u32) + 1) & 0xFFFF) as u16;
        inst_len = 2;
      }
      else if opcode.instruction == "BIT" {
        let code = Memory::read8(address);
        opcode = &opcodes::BIT_OPCODES[code as usize];
        address = (((address  as u32) + 1) & 0xFFFF) as u16;
        inst_len = 2;
      }
      else if opcode.instruction == "IX" {
        let code = Memory::read8(address);
        opcode = &opcodes::IX_OPCODES[code as usize];
        address = (((address  as u32) + 1) & 0xFFFF) as u16;
        if opcode.instruction == "IX_BIT" {
          let code = Memory::read8(address);
          opcode = &opcodes::IX_BIT_OPCODES[code as usize];
          address = (((address  as u32) + 1) & 0xFFFF) as u16;
          inst_len = 3;
        }
        else {
          inst_len = 2;
        }
      }
      else if opcode.instruction == "IY" {
        let code = Memory::read8(address);
        opcode = &opcodes::IY_OPCODES[code as usize];
        address = (((address  as u32) + 1) & 0xFFFF) as u16;
        if opcode.instruction == "IY_BIT" {
          let code = Memory::read8(address);
          opcode = &opcodes::IY_BIT_OPCODES[code as usize];
          address = (((address  as u32) + 1) & 0xFFFF) as u16;
          inst_len = 3;
        }
        else {
          inst_len = 2;
        }
      }
      let mut data: [u8;2] = [0,0];
      let n = (opcode.byte_count as usize) - inst_len;
      for i in 0..n {
        data[i] = Memory::read8(address);
        address = (((address  as u32) + 1) & 0xFFFF) as u16;
      }
      (opcode, data, n)
  }

  pub fn step() {
    let (opcode, data, n) = Self::fetch(Self::get_pc());
    write_reg16("PC", (((Self::get_pc() as u32) + (opcode.byte_count as u32)) & 0xFFFF) as u16);
    Self::run_inst(opcode.func, opcode, data, n, opcode.table, opcode.cycles);
  }

  fn run_inst(f: fn(&Instruction), opcode: &Opcode, data: [u8;2], len: usize, table: &str, cycles: (u8,u8)) {
    let inst = Instruction{code: opcode.code, 
      data: ((data[1] as u16) << 8) | (data[0] as u16), 
      len: len, table: String::from(table),
      cycles: cycles, sel_cycles: cycles.0};
    f(&inst);
    thread::sleep(Duration::from_nanos(285*(inst.sel_cycles as u64)));
  }


}





