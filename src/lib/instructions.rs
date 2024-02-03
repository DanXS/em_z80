use crate::memory::Memory;
use crate::registers::Flag;
use crate::cpu::Cpu;
use crate::cpu::*;
use crate::registers::Register;
use crate::util::*;

#[inline]
fn report_unknown(opcode_str: &str) {
  println!("Unknown {} instruction!", opcode_str);
}

#[inline]
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
  pub code: u8,
  pub data: u16,
  pub len: usize,
  pub table: String,
  pub cycles: (u8,u8)
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
    if self.table == "main" {
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
          if dst_reg == "(HL)" {
            let hl : u16 = read_reg16("HL");
            Memory::write8(hl, val);
          }
          else {
            write_reg8(dst_reg, val);
          }
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
    else if self.table == "misc" {
      if self.len == 0 {
        if self.code == 0x47 {
          // LD I,A
          let val = read_reg8("A");
          write_reg8("I", val);
        }
        else if self.code == 0x57 {
          // LD A,I
          let val = read_reg8("I");
          write_reg8("A", val);
          clear_flag(Flag::H);
          clear_flag(Flag::N);
          // ToDo: set status flags based on interrupt flags
        }
        else if self.code == 0x4F {
          // LD R,A
          let val = read_reg8("A");
          write_reg8("R", val);
        }
        else if self.code == 0x5F {
          // LD A,R
          let val = read_reg8("R");
          write_reg8("A", val);
          clear_flag(Flag::H);
          clear_flag(Flag::N);
          // ToDo: set status flags based on interrupt flags
        }
        else {
          report_unknown("LD");
        }
      }
      else if self.len == 2 {
        if self.code & 0xCF == 0x43 {
          // LD nn,(rr)
          let reg_map = ["BC", "DE", "HL", "SP"];
          let rr = (self.code & 0x30) >> 4;
          let src_data = read_reg16(reg_map[rr as usize]);
          let dst_addr = self.data;
          Memory::write16(dst_addr,src_data);
        }
        else if self.code & 0xCF == 0x4B {
          // LD rr,(nn)
          let reg_map = ["BC", "DE", "HL", "SP"];
          let rr = (self.code & 0x30) >> 4;
          let dst_reg = reg_map[rr as usize];
          let src_data = Memory::read16(self.data);
          write_reg16(dst_reg, src_data);
        }
        else {
          report_unknown("LD");
        }
      }
      else {
        report_unknown("LD");
      }
    }
    else if self.table == "ix" {
      if self.len == 1 {
        if self.code & 0xC7 == 0x46 { 
          // LD r,(IX+d)
          let reg_map = ["B", "C", "D", "E", "H", "L", "_", "A"];
          let r = (self.code & 0x38) >> 3;
          let dst_reg = reg_map[r as usize];
          if dst_reg != "_" {
            let addr = read_reg16("IX");
            let src_addr = calc_address_with_offset(addr, self.data as u8);
            let val = Memory::read8(src_addr);
            write_reg8(dst_reg, val);
          }
          else {
            report_unknown("LD");
          }
        }
        else if self.code & 0xF8 == 0x70 {
          // LD (IX+d),r
          let reg_map = ["B", "C", "D", "E", "H", "L", "_", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          if src_reg != "_" {
            let addr = read_reg16("IX");
            let dst_addr = calc_address_with_offset(addr, self.data as u8);
            let val = read_reg8(src_reg);
            Memory::write8(dst_addr, val);
          }
          else {
            report_unknown("LD");
          }
        }
        else {
          report_unknown("LD");
        }
      }
      else if self.len == 2 {
          // LD (IX+d),n
          let d = self.data as u8;
          let val = ((self.data >> 8) & 0x00FF) as u8;
          let addr = read_reg16("IX");
          let dst_addr = calc_address_with_offset(addr, d);
          Memory::write8(dst_addr, val);
      }
      else {
        report_not_supported("LD");
      }
    }
    else if self.table == "iy" {
      if self.len == 1 {
        if self.code & 0xC7 == 0x46 { 
          // LD r,(IY+d)
          let reg_map = ["B", "C", "D", "E", "H", "L", "_", "A"];
          let r = (self.code & 0x38) >> 3;
          let dst_reg = reg_map[r as usize];
          if dst_reg != "_" {
            let addr = read_reg16("IY");
            let src_addr = calc_address_with_offset(addr, self.data as u8);
            let val = Memory::read8(src_addr);
            write_reg8(dst_reg, val);
          }
          else {
            report_unknown("LD");
          }
        }
        else if self.code & 0xF8 == 0x70 {
          // LD (IY+d),r
          let reg_map = ["B", "C", "D", "E", "H", "L", "_", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          if src_reg != "_" {
            let addr = read_reg16("IY");
            let dst_addr = calc_address_with_offset(addr, self.data as u8);
            let val = read_reg8(src_reg);
            Memory::write8(dst_addr, val);
          }
          else {
            report_unknown("LD");
          }
        }
        else {
          report_unknown("LD");
        }
      }
      else if self.len == 2 {
        if self.code == 0x36 {
          // LD (IY+d),n
          let d = self.data as u8;
          let val = ((self.data >> 8) & 0x00FF) as u8;
          let addr = read_reg16("IY");
          let dst_addr = calc_address_with_offset(addr, d);
          Memory::write8(dst_addr, val);
        }
      }
      else {
        report_unknown("LD");
      }
    }
    else {
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
  fn inc(&self) {
    if self.table == "main" {
      if self.code & 0xC7 == 0x04 {
        // INC r
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        let dst = ((self.code & 0x38) >> 3) as usize;
        let reg = reg_map[dst];
        if reg == "(HL)" {
          let addr = read_reg16("HL");
          let val = Memory::read8(addr);
          let inc_val = inc_u8_wrap(val);
          Memory::write8(addr, inc_val);
          update_flags_for_inc(val, inc_val);
        }
        else  {
          let val = read_reg8(reg);
          let inc_val = inc_u8_wrap(val);
          write_reg8(reg, inc_val);
          update_flags_for_inc(val, inc_val);
        }
      }
      else if self.code & 0xCF == 0x03 {
        // INC rr
        let reg_map = ["BC", "DE", "HL", "SP"];
        let dst = ((self.code & 0x30) >> 4) as usize;
        let reg = reg_map[dst];
        let val = read_reg16(reg);
        write_reg16(reg, inc_u16_wrap(val));
      }
      else {
        report_unknown("INC");
      }
    }
    else if self.table == "ix" {
      if self.len == 0 && self.code == 0x23 {
        // INC IX
        let val = read_reg16("IX");
        write_reg16("IX", inc_u16_wrap(val));
      }
      if self.len == 1 && self.code == 0x34 {
        // INC (IX + d)
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val = Memory::read8(src_addr);
        let inc_val = inc_u8_wrap(val);
        Memory::write8(src_addr, inc_val);
        update_flags_for_inc(val, inc_val);
      }
      else {
        report_unknown("INC")
      }
    }
    else if self.table == "iy" {
      if self.len == 0 && self.code == 0x23 {
        // INC IY
        let val = read_reg16("IY");
        write_reg16("IY", inc_u16_wrap(val));
      }
      else if self.len == 1 && self.code == 0x34 {
        // INC (IY + d)
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val = Memory::read8(src_addr);
        let inc_val = inc_u8_wrap(val);
        Memory::write8(src_addr, inc_val);
        update_flags_for_inc(val, inc_val);
      }
      else {
        report_unknown("INC")
      }
    }
    else {
      report_unknown("INC");
    }
  }

  // DEC instruction
  fn dec(&self) {
    if self.table == "main" {
      if self.code & 0xC7 == 0x05 {
        let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
        let dst = ((self.code & 0x38) >> 3) as usize;
        let reg = reg_map[dst];
        if reg == "(HL)" {
          // DEC (HL)
          let addr = read_reg16("HL");
          let val = Memory::read8(addr);
          let dec_val = dec_u8_wrap(val);
          Memory::write8(addr, dec_val);
          update_flags_for_dec(val, dec_val);
        }
        else  {
          // DEC r
          let val = read_reg8(reg);
          let dec_val = dec_u8_wrap(val);
          write_reg8(reg, dec_val);
          update_flags_for_dec(val, dec_val);
        }
      }
      else if self.code & 0xCF == 0x0B {
        // DEC rr
        let reg_map = ["BC", "DE", "HL", "SP"];
        let dst = ((self.code & 0x30) >> 4) as usize;
        let reg = reg_map[dst];
        let val = read_reg16(reg);
        write_reg16(reg, dec_u16_wrap(val));
      }
      else {
        report_unknown("DEC");
      }
    }
    else if self.table == "ix" {
      if self.len == 0 && self.code == 0x2B {
        // DEX IX
        let val = read_reg16("IX");
        write_reg16("IX", dec_u16_wrap(val));
      }
      if self.len == 1 && self.code == 0x34 {
        // DEC (IX + d)
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val = Memory::read8(src_addr);
        let dec_val = dec_u8_wrap(val);
        Memory::write8(src_addr, dec_val);
        update_flags_for_dec(val,dec_val);
      }
      else {
        report_unknown("INC")
      }
    }
    else if self.table == "iy" {
      if self.len == 0 && self.code == 0x2B {
        // DEX IY
        let val = read_reg16("IY");
        write_reg16("IY", dec_u16_wrap(val));
      }
      if self.len == 1 && self.code == 0x34 {
        // DEC (IY + d)
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val = Memory::read8(src_addr);
        let dec_val = dec_u8_wrap(val);
        Memory::write8(src_addr, dec_val);
        update_flags_for_dec(val, dec_val);
      }
      else {
        report_unknown("INC")
      }
    }
    else {
      report_unknown("DEC");
    }
  }

  // PUSH Instruction
  fn push(&self) {
    if self.table == "main" && self.code & 0xCF == 0xC5 {
      // PUSH rr
      let reg_map = ["BC", "DE", "HL", "SP"];
      let src = ((self.code & 0x30) >> 4) as usize;
      let src_reg = reg_map[src];
      let val = read_reg16(src_reg);
      let mut addr = read_reg16("SP");
      addr = ((addr as i32) - 2) as u16;
      write_reg16("SP", addr);
      Memory::write16(addr, val);
    }
    else if self.table == "ix" && self.code == 0xE5 {
      // PUSH IX
      let val = read_reg16("IX");
      let mut addr = read_reg16("SP");
      addr = ((addr as i32) - 2) as u16;
      write_reg16("SP", addr);
      Memory::write16(addr, val);
    }
    else if self.table == "iy" && self.code == 0xE5 {
      // PUSH IY
      let val = read_reg16("IY");
      let mut addr = read_reg16("SP");
      addr = ((addr as i32) - 2) as u16;
      write_reg16("SP", addr);
      Memory::write16(addr, val);
    }
    else {
      report_unknown("PUSH");
    }
  }

  // POP Instruction
  fn pop(&self) {
    if self.table == "main" && self.code & 0xCF == 0xC1 {
      // POP rr
      let reg_map = ["BC", "DE", "HL", "SP"];
      let rr = ((self.code & 0x30) >> 4) as usize;
      let dst_reg = reg_map[rr];
      let mut addr = read_reg16("SP");
      let val = Memory::read16(addr);
      write_reg16(dst_reg, val);
      addr = ((addr as u32) + 2) as u16;
      write_reg16("SP", addr);
    }
    else if self.table == "ix" && self.code == 0xE1 {
      // POP IX
      let mut addr = read_reg16("SP");
      let val = Memory::read16(addr);
      write_reg16("IX", val);
      addr = ((addr as u32) + 2) as u16;
      write_reg16("SP", addr);
    }
    else if self.table == "iy" && self.code == 0xE1 {
      // POP IY
      let mut addr = read_reg16("SP");
      let val = Memory::read16(addr);
      write_reg16("IY", val);
      addr = ((addr as u32) + 2) as u16;
      write_reg16("SP", addr);
    }
    else {
      report_unknown("POP");
    }
  }

  // ADD Instruction
  fn add(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0x80 {
          // ADD A,r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8("A");
          let val2 = {
            if src_reg == "(HL)" {
              let src_addr = read_reg16("HL");
              Memory::read8(src_addr)
            }
            else {
              read_reg8(src_reg)
            }
          };
          let res = (val1 as u16) + (val2 as u16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_addition8(val1, val2, res);
        }
        else if self.code & 0xCF == 0x09 {
          // ADD HL,rr
          let reg_map = ["BE", "DE", "HL", "SP"];
          let rr = (self.code & 0x30) >> 4;
          let src_reg = reg_map[rr as usize];
          let val1 = read_reg16("HL");
          let val2 = read_reg16(src_reg);
          let res = (val1 as u32) + (val2 as u32);
          write_reg16("HL", (res & 0xFFFF) as u16);
          update_flags_for_addition16(val1, res);
        }
        else {
          report_unknown("ADD");
        }
      }
      else if self.len == 1 {
        if self.code == 0xC6 {
          // ADD A,n
          let val1 = read_reg8("A");
          let val2 = ((self.data >> 8) & 0xFF) as u8;
          let res = (val1 as u16) + (val2 as u16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_addition8(val1, val2, res);
        }
        else {
          report_unknown("ADD");
        }
      }
      else {
        report_unknown("ADD");
      }
    }
    else if self.table == "ix" {
      if self.code == 0x86 {
        // ADD A, (IX + d)
        let val1 = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as u16) + (val2 as u16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_addition8(val1, val2, res);
      }
      else if self.code & 0xCF == 0x09 {
        // ADD IX, rr
        let val1 = read_reg16("IX");
        let reg_map = ["BC", "DE", "IX", "SP"];
        let rr = self.code & 0x30;
        let val2 = read_reg16(reg_map[rr as usize]);
        let res = (val1 as u32) + (val2 as u32);
        write_reg16("IX", (res & 0xFFFF) as u16);
        update_flags_for_addition16(val1, res);
      }
      else {
        report_unknown("ADD");
      }
    }
    else if self.table == "iy" {
      if self.code == 0x86 {
        // ADD A, (IY + d)
        let val1 = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as u16) + (val2 as u16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_addition8(val1, val2, res);
      }
      else if self.code & 0xCF == 0x09 {
        // ADD IY, rr
        let val1 = read_reg16("IY");
        let reg_map = ["BC", "DE", "IY", "SP"];
        let rr = self.code & 0x30;
        let val2 = read_reg16(reg_map[rr as usize]);
        let res = (val1 as u32) + (val2 as u32);
        write_reg16("IY", (res & 0xFFFF) as u16);
        update_flags_for_addition16(val1, res);
      }
      else {
        report_unknown("ADD");
      }
    }
    else {
      report_unknown("ADD");
    }
  }

  // ADC Instruction
  fn adc(&self) {
    let carry: u8 = {
      if is_flag_set(Flag::C) { 1 }
      else { 0 }
    };
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0x88 {
          // ADC A,r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8("A");
          let val2 = {
            if src_reg == "(HL)" {
              let src_addr = read_reg16("HL");
              Memory::read8(src_addr)
            }
            else {
              read_reg8(src_reg)
            }
          };
          let res = (val1 as u16) + (val2 as u16) + (carry as u16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
        }
        else {
          report_unknown("ADC");
        }
      }
      else if self.len == 1 {
        if self.code == 0xCE {
          // ADC A,n
          let val1 = read_reg8("A");
          let val2 = ((self.data >> 8) & 0xFF) as u8;
          let res = (val1 as u16) + (val2 as u16) + (carry as u16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
        }
        else {
          report_unknown("ADC");
        }
      }
      else {
        report_unknown("ADC");
      }
    }
    else if self.table == "misc" {
      if self.code & 0xCF == 0x4A {
        // ADC HL,rr
        let reg_map = ["BE", "DE", "HL", "SP"];
        let rr = (self.code & 0x30) >> 4;
        let src_reg = reg_map[rr as usize];
        let val1 = read_reg16("HL");
        let val2 = read_reg16(src_reg);
        let res = (val1 as u32) + (val2 as u32) + (carry as u32);
        write_reg16("HL", (res & 0xFFFF) as u16);
        update_flags_for_addition_with_carry16(val1, val2, res);
      }
      else {
        report_unknown("ADC");
      }
    }
    else if self.table == "ix" {
      // ADC A, (IX + d)
      if self.code == 0x8E {
        let val1 = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as u16) + (val2 as u16) + (carry as u16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
      }
    }
    else if self.table == "iy" {
      // ADC A, (IY + d)
      if self.code == 0x8E {
        let val1 = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as u16) + (val2 as u16) + (carry as u16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
      }
    }
    else {
      report_unknown("ADC");
    }
  }

  // SUB Instruction
  fn sub(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0x90 {
          // SUB A,r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8("A");
          let val2 = {
            if src_reg == "(HL)" {
              let src_addr = read_reg16("HL");
              Memory::read8(src_addr)
            }
            else {
              read_reg8(src_reg)
            }
          };
          let res = (val1 as i16) - (val2 as i16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, val2, res as u16);
        }
        else {
          report_unknown("SUB");
        }
      }
      else if self.len == 1 {
        if self.code == 0xD6 {
          // SUB A,n
          let val1 = read_reg8("A");
          let val2 = ((self.data >> 8) & 0xFF) as u8;
          let res = (val1 as i16) - (val2 as i16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, val2, res as u16);
        }
        else {
          report_unknown("SUB");
        }
      }
      else {
        report_unknown("SUB");
      }
    }
    else if self.table == "ix" {
      // SUB A, (IX + d)
      if self.code == 0x96 {
        let val1 = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as i16) - (val2 as i16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_subtraction8(val1, val2, res as u16);
      }
    }
    else if self.table == "iy" {
      // SUB A, (IY + d)
      if self.code == 0x96 {
        let val1 = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as i16) - (val2 as i16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_subtraction8(val1, val2, res as u16);
      }
    }
    else {
      report_unknown("SUB");
    }
  }

  // SBC Instruction
  fn sbc(&self) {
    let carry: u8 = {
      if is_flag_set(Flag::C) { 1 }
      else { 0 }
    };
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0x98 {
          // SBC A,r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8("A");
          let val2 = {
            if src_reg == "(HL)" {
              let src_addr = read_reg16("HL");
              Memory::read8(src_addr)
            }
            else {
              read_reg8(src_reg)
            }
          };
          let res = (val1 as i16) - (val2 as i16) - (carry as i16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
        }
        else {
          report_unknown("SBC");
        }
      }
      else if self.len == 1 {
        if self.code == 0xDE {
          // SBC A,n
          let val1 = read_reg8("A");
          let val2 = ((self.data >> 8) & 0xFF) as u8;
          let res = (val1 as i16) - (val2 as i16) - (carry as i16);
          write_reg8("A", (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
        }
        else {
          report_unknown("SBC");
        }
      }
      else {
        report_unknown("SBC");
      }
    }
    else if self.table == "misc" {
      if self.code & 0xCF == 0x42 {
        // SDC HL,rr
        let reg_map = ["BE", "DE", "HL", "SP"];
        let rr = (self.code & 0x30) >> 4;
        let src_reg = reg_map[rr as usize];
        let val1 = read_reg16("HL");
        let val2 = read_reg16(src_reg);
        let res = (val1 as i32) - (val2 as i32) - (carry as i32);
        write_reg16("HL", (res & 0xFFFF) as u16);
        update_flags_for_subtraction_with_carry16(val1, val2, res as u32);
      }
      else {
        report_unknown("ADC");
      }
    }
    else if self.table == "ix" {
      // SBC A, (IX + d)
      if self.code == 0x9E {
        let val1 = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as i16) - (val2 as i16) - (carry as i16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
      }
    }
    else if self.table == "iy" {
      // SBC A, (IY + d)
      if self.code == 0x9E {
        let val1 = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        let res = (val1 as i16) - (val2 as i16) - (carry as i16);
        write_reg8("A", (res & 0xFF) as u8);
        update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
      }
    }
    else {
      report_unknown("SBC");
    }
  }

  // NEG Instruction
  fn neg(&self) {
    if self.table == "misc" {
      if self.code == 0x44 {
        let val = read_reg8("A");
        let res = -(val as i16);
        write_reg8("A", res as u8);
        update_flags_for_negation8(val, res as u16);
      }
      else {
        report_unknown("NEG");
      }
    }
    else {
      report_unknown("NEG");
    }
  }

  // AND Instruction
  fn and(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0xA0 {
          // AND r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8("A");
          if src_reg == "(HL)" {
            // AND (HL)
            let src_addr = read_reg16("HL");
            let src_val = Memory::read8(src_addr);
            let res = val & src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, true);
          }
          else {
            let src_val = read_reg8(src_reg);
            let res = val & src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, true);
          }
        }
        else {
          report_unknown("AND");
        }
      }
      else if self.len == 1 {
        if self.code == 0xE6 {
          // AND n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8("A");
          let res = val & src_val;
          write_reg8("A", res);
          update_flags_for_logical_op(res, true);
        }
        else {
          report_unknown("AND");
        }
      }
      else {
        report_unknown("AND");
      }
    }
    else if self.table == "ix" && self.len == 1 {
      if self.code == 0xA6 {
        // AND IX+d
        let val = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val & src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, true);
      }
      else {
        report_unknown("AND");
      }
    }
    else if self.table == "iy" && self.len == 1 {
      if self.code == 0xA6 {
        // AND IY+d
        let val = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val & src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, true);
      }
      else {
        report_unknown("AND");
      }
    }
    else {
      report_unknown("AND");
    }
  }

  // OR Instruction
  fn or(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0xB0 {
          // OR r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8("A");
          if src_reg == "(HL)" {
            // OR (HL)
            let src_addr = read_reg16("HL");
            let src_val = Memory::read8(src_addr);
            let res = val | src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, false);
          }
          else {
            let src_val = read_reg8(src_reg);
            let res = val | src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, false);
          }
        }
        else {
          report_unknown("OR");
        }
      }
      else if self.len == 1 {
        if self.code == 0xF6 {
          // OR n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8("A");
          let res = val | src_val;
          write_reg8("A", res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("OR");
        }
      }
      else {
        report_unknown("OR");
      }
    }
    else if self.table == "ix" && self.len == 1 {
      if self.code == 0xB6 {
        // OR IX+d
        let val = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val | src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, false);
      }
      else {
        report_unknown("OR");
      }
    }
    else if self.table == "iy" && self.len == 1 {
      if self.code == 0xB6 {
        // OR IY+d
        let val = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val | src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, false);
      }
      else {
        report_unknown("OR");
      }
    }
    else {
      report_unknown("OR");
    }
  }

  // XOR Instruction
  fn xor(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0xA8 {
          // XOR r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8("A");
          if src_reg == "(HL)" {
            // XOR (HL)
            let src_addr = read_reg16("HL");
            let src_val = Memory::read8(src_addr);
            let res = val ^ src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, false);
          }
          else {
            let src_val = read_reg8(src_reg);
            let res = val ^ src_val;
            write_reg8("A", res);
            update_flags_for_logical_op(res, false);
          }
        }
        else {
          report_unknown("XOR");
        }
      }
      else if self.len == 1 {
        if self.code == 0xEE {
          // XOR n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8("A");
          let res = val ^ src_val;
          write_reg8("A", res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("XOR");
        }
      }
      else {
        report_unknown("XOR");
      }
    }
    else if self.table == "ix" && self.len == 1 {
      if self.code == 0xAE {
        // XOR IX+d
        let val = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val ^ src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, false);
      }
      else {
        report_unknown("XOR");
      }
    }
    else if self.table == "iy" && self.len == 1 {
      if self.code == 0xAE {
        // XOR IY+d
        let val = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let src_val = Memory::read8(src_addr);
        let res = val ^ src_val;
        write_reg8("A", res);
        update_flags_for_logical_op(res, false);
      }
      else {
        report_unknown("XOR");
      }
    }
    else {
      report_unknown("XOR");
    }
  }

  // CP Instruction
  fn cp(&self) {
    if self.table == "main" {
      if self.len == 0 {
        if self.code & 0xF8 == 0xB8 {
          // CP r
          let reg_map = ["B", "C", "D", "E", "H", "L", "(HL)", "A"];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8("A");
          if src_reg == "(HL)" {
            // CP (HL)
            let src_addr = read_reg16("HL");
            let val2 = Memory::read8(src_addr);
            update_flags_for_compare8(val1, val2);
          }
          else {
            let val2 = read_reg8(src_reg);
            update_flags_for_compare8(val1, val2);
          }
        }
        else {
          report_unknown("CP");
        }
      }
      else if self.len == 1 {
        if self.code == 0xFE {
          // CP n
          let val1 = read_reg8("A");
          let val2 = (self.data & 0xff) as u8;
          update_flags_for_compare8(val1, val2);
        }
        else {
          report_unknown("CP");
        }
      }
      else {
        report_unknown("CP");
      }
    }
    else if self.table == "ix" && self.len == 1 {
      if self.code == 0xBE {
        // CP IX+d
        let val1 = read_reg8("A");
        let addr = read_reg16("IX");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        update_flags_for_compare8(val1, val2);
      }
      else {
        report_unknown("CP");
      }
    }
    else if self.table == "iy" && self.len == 1 {
      if self.code == 0xBE {
        // CP IY+d
        let val1 = read_reg8("A");
        let addr = read_reg16("IY");
        let src_addr = calc_address_with_offset(addr, self.data as u8);
        let val2 = Memory::read8(src_addr);
        update_flags_for_compare8(val1, val2);
      }
      else {
        report_unknown("CP");
      }
    }
    else {
      report_unknown("CP");
    }
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

  // JP Instruction
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
        let cc_val = (self.code & 0x38) >> 3;
        let cc = Register::condition_code_for(cc_val);
        let flags = read_reg8("F");
        let should_jump = Register::check_condition(cc, flags);
        if should_jump {
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else if self.table == "ix" {
        if self.code == 0xE9 {
          // JP (IX)
          let hl = read_reg16("IX");
          let dst_addr = Memory::read16(hl);
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else if self.table == "iy" {
        if self.code == 0xE9 {
          // JP (IY)
          let hl = read_reg16("IY");
          let dst_addr = Memory::read16(hl);
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
    }
    else {
      report_unknown("JP");
    }
  }

  // JR Instruction
  fn jr(&self) {
    if self.table == "main" {
      let addr = Cpu::get_register16("PC");
      let dst_addr = calc_address_with_offset(addr, self.data as u8);
      if self.code == 0x18 {
        // JR d
        write_reg16("PC", dst_addr);
      }
      else if self.code == 0x20 {
        // JR NZ
        if !is_flag_set(Flag::Z) {
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else if self.code == 0x28 {
        // JR Z
        if is_flag_set(Flag::Z) {
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else if self.code == 0x30 {
        // JR NC
        if !is_flag_set(Flag::C) {
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else if self.code == 0x38 {
        // JR C
        if is_flag_set(Flag::C) {
          write_reg16("PC", dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
      }
      else {
        report_unknown("JR");
      }
    }
    else {
      report_unknown("JR");
    }
  }

  // CALL Instruction
  fn call(&self) {
    if self.table == "main" {
      if self.len == 2 {
        if self.code == 0xCD { 
          // CALL nn
          let val = read_reg16("PC");
          let mut addr = read_reg16("SP");
          addr = ((addr as i32) - 2) as u16;
          write_reg16("SP", addr);
          Memory::write16(addr, val);
          let dst_addr = self.data;
          write_reg16("PC", dst_addr);
        }
        else if self.code & 0xC7 == 0xC4 {
          // CALL cc,nn
          let cc_val = (self.code & 0x38) >> 3;
          let cc = Register::condition_code_for(cc_val);
          let flags = read_reg8("F");
          let should_call = Register::check_condition(cc, flags);
          if should_call {
            let val = read_reg16("PC");
            let mut addr = read_reg16("SP");
            addr = ((addr as i32) - 2) as u16;
            write_reg16("SP", addr);
            Memory::write16(addr, val);
            let dst_addr = self.data;
            write_reg16("PC", dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
        }
        else {
          report_unknown("CALL");
        }
      }
      else {
        report_unknown("CALL");
      }
    }
    else {
      report_unknown("CALL");
    }
  }

  fn djnz(&self) {
    report_unknown("DJNZ");
  }

  // RET Instruciton
  fn ret(&self) {
    if self.table == "main" && self.len == 0 {
      if self.code == 0xC9 {
        // RET
        let sp = read_reg16("SP");
        let pc = Memory::read16(sp);
        write_reg16("SP", ((sp as u32) + 2) as u16);
        write_reg16("PC", pc);
      }
      else if self.code & 0xC7 == 0xC0 {
        // RET cc
        let cc_val = (self.code & 0x38) >> 3;
        let cc = Register::condition_code_for(cc_val);
        let flags = read_reg8("F");
        let should_return = Register::check_condition(cc, flags);
        if should_return {
          let sp = read_reg16("SP");
          let pc = Memory::read16(sp);
          write_reg16("SP", ((sp as u32) + 2) as u16);
          write_reg16("PC", pc);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
        else {
          Cpu::set_acitve_cycles(self.cycles.1);
        }
      }
      else {
        report_unknown("RET");
      }
    }
    else {
      report_unknown("RET");
    }
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

  // LDI Instruction
  fn ldi(&self) {
    if self.table == "misc" {
      if self.code == 0xA0 {
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        let mut bc = read_reg16("BC");
        let val = Memory::read8(hl);
        Memory::write8(de, val);
        write_reg16("DE", inc_u16_wrap(de));
        write_reg16("HL", inc_u16_wrap(hl));
        bc = dec_u16_wrap(bc);
        write_reg16("BC", bc);
        clear_flag(Flag::H);
        clear_flag(Flag::N);
        if dec_u16_wrap(bc) != 0x0000 {
          set_flag(Flag::PV);
        }
        else {
          clear_flag(Flag::PV);
        }
      }
      else {
        report_unknown("LDI");
      }
    }
    else {
      report_unknown("LDI");
    }
  }

  // LDIR Instruction
  fn ldir(&self) {
    if self.table == "misc" {
      if self.code == 0xB0 {
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        let mut bc = read_reg16("BC");
        let val = Memory::read8(hl);
        Memory::write8(de, val);
        write_reg16("DE", inc_u16_wrap(de));
        write_reg16("HL", inc_u16_wrap(hl));
        bc = dec_u16_wrap(bc);
        write_reg16("BC", bc);
        if bc != 0x00 {
          let mut pc = read_reg16("PC");
          pc = calc_address_with_offset(pc, 0xfe);
          write_reg16("PC", pc);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
        else {
          Cpu::set_acitve_cycles(self.cycles.1);
        }
        clear_flag(Flag::H);
        clear_flag(Flag::N);
        if dec_u16_wrap(bc) != 0x0000 {
          set_flag(Flag::PV);
        }
        else {
          clear_flag(Flag::PV);
        }
      }
      else {
        report_unknown("LDIR");
      }
    }
    else {
      report_unknown("LDIR");
    }
  }

  // LDD
  fn ldd(&self) {
    if self.table == "misc" {
      if self.code == 0xA8 {
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        let mut bc = read_reg16("BC");
        let val = Memory::read8(hl);
        Memory::write8(de, val);
        write_reg16("DE", dec_u16_wrap(de));
        write_reg16("HL", dec_u16_wrap(hl));
        bc = dec_u16_wrap(bc);
        write_reg16("BC", bc);
        clear_flag(Flag::H);
        clear_flag(Flag::N);
        if dec_u16_wrap(bc) != 0x0000 {
          set_flag(Flag::PV);
        }
        else {
          clear_flag(Flag::PV);
        }
      }
      else {
        report_unknown("LDD");
      }
    }
    else {
      report_unknown("LDD");
    }
  }

  // LDDR Instruction
  fn lddr(&self) {
    if self.table == "misc" {
      if self.code == 0xB8 {
        let de = read_reg16("DE");
        let hl = read_reg16("HL");
        let mut bc = read_reg16("BC");
        let val = Memory::read8(hl);
        Memory::write8(de, val);
        write_reg16("DE", dec_u16_wrap(de));
        write_reg16("HL", dec_u16_wrap(hl));
        bc = dec_u16_wrap(bc);
        write_reg16("BC", bc);
        if bc != 0x00 {
          let mut pc = read_reg16("PC");
          pc = calc_address_with_offset(pc, 0xfe);
          write_reg16("PC", pc);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
        else {
          Cpu::set_acitve_cycles(self.cycles.1);
        }
        clear_flag(Flag::H);
        clear_flag(Flag::N);
        clear_flag(Flag::PV);
      }
      else {
        report_unknown("LDDR");
      }
    }
    else {
      report_unknown("LDDR");
    }
  }


  fn cpi(&self) {
    report_unknown("CPI");
  }

  fn cpd(&self) {
    report_unknown("CPD");
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

  fn mlt(&self) {
    report_unknown("MLT");
  }

  fn rrd(&self) {
    report_unknown("RRD");
  }

  fn rld(&self) {
    report_unknown("RLD");
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



  // The following are unsupported on the z80, but are legal for the z180 should we wish
  // to implement them later, keeping stubs to warn user if encountered
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

