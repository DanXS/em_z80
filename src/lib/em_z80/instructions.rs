use crate::memory::Memory;
use crate::opcodes::TableID;
use crate::registers::Flag;
use crate::cpu::Cpu;
use crate::cpu::*;
use crate::registers::Register;
use crate::registers::RegID;
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
  pub table: TableID,
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
    match self.table {
      TableID::MAIN => {
        if self.len == 0 {
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          if self.code & 0xC0 == 0x40 {
            // LD r, r
            let src = (self.code & 0x07) as usize;
            let dst = ((self.code >> 3) & 0x07) as usize;
            let src_reg = reg_map[src];
            let dst_reg = reg_map[dst];
            if matches!(src_reg, RegID::HL) {
              let hl : u16 = read_reg16(RegID::HL);
              let val = Memory::read8(hl);
              write_reg8(dst_reg, val);
            }
            else if matches!(dst_reg,RegID::HL) {
              let val = read_reg8(src_reg);
              let hl : u16 = read_reg16(RegID::HL);
              Memory::write8(hl, val);
            }
            else {
              let val = read_reg8(src_reg);
              write_reg8(dst_reg, val);
            }
          }
          else if self.code == 0x02 {
            // LD (BC), A
            let val = read_reg8(RegID::A);
            let bc : u16 = read_reg16(RegID::BC);
            Memory::write8(bc, val);
          }
          else if self.code == 0x12 {
            // LD (DE), A
            let val = read_reg8(RegID::A);
            let de : u16 = read_reg16(RegID::DE);
            Memory::write8(de, val);
          }
          else if self.code == 0x0A {
            // LD A, (BC)
            let bc : u16 = read_reg16(RegID::BC);
            let val = Memory::read8(bc);
            write_reg8(RegID::A, val);
          }
          else if self.code == 0x1A {
            // LD A, (DC)
            let de : u16 = read_reg16(RegID::DE);
            let val = Memory::read8(de);
            write_reg8(RegID::A, val);
          }
          else if self.code == 0xF9 {
            // LD SP, HL
            let hl : u16 = read_reg16(RegID::HL);
            write_reg16(RegID::SP, hl);
          }
          else {
            report_unknown("LD");
          }
        }
        else if self.len == 1 {
          // LD r, n
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          if self.code & 0xC7 == 0x06 {
            let val = self.data as u8;
            let dst = ((self.code & 0x38) >> 3) as usize;
            let dst_reg = reg_map[dst];
            if matches!(dst_reg, RegID::HL) {
              let hl : u16 = read_reg16(RegID::HL);
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
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
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
            let val = read_reg16(RegID::HL);
            Memory::write16(dst_addr, val);
          }
          else if self.code == 0x2A {
            // LD HL, (nn)
            let src_addr = self.data;
            let val = Memory::read16(src_addr);
            write_reg16(RegID::HL, val);
          }
          else if self.code == 0x32 {
            // LD (nn), A
            let dst_addr = self.data;
            let val = read_reg8(RegID::A);
            Memory::write8(dst_addr, val);
          }
          else if self.code == 0x3A {
            // LD A, (nn)
            let src_addr = self.data;
            let val = Memory::read8(src_addr);
            write_reg8(RegID::A, val);
          }
          else {
            report_unknown("LD");
          }
        }
      },
      TableID::MISC => {
        if self.len == 0 {
          if self.code == 0x47 {
            // LD I,A
            let val = read_reg8(RegID::A);
            write_reg8(RegID::I, val);
          }
          else if self.code == 0x57 {
            // LD A,I
            let val = read_reg8(RegID::I);
            write_reg8(RegID::A, val);
            clear_flag(Flag::H);
            clear_flag(Flag::N);
            // ToDo: set status flags based on interrupt flags
          }
          else if self.code == 0x4F {
            // LD R,A
            let val = read_reg8(RegID::A);
            write_reg8(RegID::R, val);
          }
          else if self.code == 0x5F {
            // LD A,R
            let val = read_reg8(RegID::R);
            write_reg8(RegID::A, val);
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
            let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
            let rr = (self.code & 0x30) >> 4;
            let src_data = read_reg16(reg_map[rr as usize]);
            let dst_addr = self.data;
            Memory::write16(dst_addr,src_data);
          }
          else if self.code & 0xCF == 0x4B {
            // LD rr,(nn)
            let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
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
      },
      TableID::IX => {
        if self.len == 1 {
          if self.code & 0xC7 == 0x46 { 
            // LD r,(IX+d)
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = (self.code & 0x38) >> 3;
            let dst_reg = reg_map[r as usize];
            if !matches!(dst_reg, RegID::HL) {
              let addr = read_reg16(RegID::IX);
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
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            if !matches!(src_reg, RegID::HL) {
              let addr = read_reg16(RegID::IX);
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
            // LD (IX+d),n
            let d = self.data as u8;
            let val = ((self.data >> 8) & 0x00FF) as u8;
            let addr = read_reg16(RegID::IX);
            let dst_addr = calc_address_with_offset(addr, d);
            Memory::write8(dst_addr, val);
          }
          else if self.code == 0x21 {
            // LD IX,nn
            write_reg16(RegID::IX, self.data);
          }
          else {
            report_unknown("LD");
          }
        }
        else {
          report_not_supported("LD");
        }
      },
      TableID::IY => {
        if self.len == 1 {
          if self.code & 0xC7 == 0x46 { 
            // LD r,(IY+d)
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = (self.code & 0x38) >> 3;
            let dst_reg = reg_map[r as usize];
            if !matches!(dst_reg, RegID::HL) {
              let addr = read_reg16(RegID::IY);
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
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            if !matches!(src_reg, RegID::HL) {
              let addr = read_reg16(RegID::IY);
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
            let addr = read_reg16(RegID::IY);
            let dst_addr = calc_address_with_offset(addr, d);
            Memory::write8(dst_addr, val);
          }
          else if self.code == 0x21 {
            // LD IX,nn
            write_reg16(RegID::IY, self.data);
          }
          else {
            report_unknown("LD");
          }
        }
        else {
          report_unknown("LD");
        }
      },
      _ => report_unknown("LD")
    }
  }

  fn ex(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code == 0xEB {
          // EX DE, HL
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          write_reg16(RegID::HL, de);
          write_reg16(RegID::DE, hl);
        }
        else if self.code == 0x08 {
          // EX AF, AF'
          let af = read_reg16(RegID::AF);
          let af_p = read_reg16(RegID::AFP);
          write_reg16(RegID::AFP, af);
          write_reg16(RegID::AF, af_p);
        }
        else if self.code == 0xE3 {
          // EX (SP), HL
          let sp = read_reg16(RegID::SP);
          let hl = read_reg16(RegID::HL);
          let data = Memory::read16(sp);
          write_reg16(RegID::HL, data);
          Memory::write16(sp, hl);
        }
        else {
          report_unknown("EX");
        }
      },
      _ => report_unknown("EX")
    }
  }

  fn exx(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code == 0xD9 {
          // EXX
          let bc = read_reg16(RegID::BC);
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          let bc_p = read_reg16(RegID::BCP);
          let de_p = read_reg16(RegID::DEP);
          let hl_p = read_reg16(RegID::HLP);
          write_reg16(RegID::BC, bc_p);
          write_reg16(RegID::DE, de_p);
          write_reg16(RegID::HL, hl_p);
          write_reg16(RegID::BCP, bc);
          write_reg16(RegID::DEP, de);
          write_reg16(RegID::HLP, hl);
        }
        else {
          report_unknown("EXX");
        }
      },
      _ => report_unknown("EXX")
    }
  }

  // INC instruction
  fn inc(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code & 0xC7 == 0x04 {
          // INC r
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let dst = ((self.code & 0x38) >> 3) as usize;
          let reg = reg_map[dst];
          if matches!(reg, RegID::HL) {
            let addr = read_reg16(RegID::HL);
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
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let dst = ((self.code & 0x30) >> 4) as usize;
          let reg = reg_map[dst];
          let val = read_reg16(reg);
          write_reg16(reg, inc_u16_wrap(val));
        }
        else {
          report_unknown("INC");
        }
      },
      TableID::IX => {
        if self.len == 0 && self.code == 0x23 {
          // INC IX
          let val = read_reg16(RegID::IX);
          write_reg16(RegID::IX, inc_u16_wrap(val));
        }
        if self.len == 1 && self.code == 0x34 {
          // INC (IX + d)
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val = Memory::read8(src_addr);
          let inc_val = inc_u8_wrap(val);
          Memory::write8(src_addr, inc_val);
          update_flags_for_inc(val, inc_val);
        }
        else {
          report_unknown("INC")
        }
      },
      TableID::IY => {
        if self.len == 0 && self.code == 0x23 {
          // INC IY
          let val = read_reg16(RegID::IY);
          write_reg16(RegID::IY, inc_u16_wrap(val));
        }
        else if self.len == 1 && self.code == 0x34 {
          // INC (IY + d)
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val = Memory::read8(src_addr);
          let inc_val = inc_u8_wrap(val);
          Memory::write8(src_addr, inc_val);
          update_flags_for_inc(val, inc_val);
        }
        else {
          report_unknown("INC")
        }
      },
      _ => report_unknown("INC")
    }
  }

  // DEC instruction
  fn dec(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code & 0xC7 == 0x05 {
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let dst = ((self.code & 0x38) >> 3) as usize;
          let reg = reg_map[dst];
          if matches!(reg, RegID::HL) {
            // DEC (HL)
            let addr = read_reg16(RegID::HL);
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
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let dst = ((self.code & 0x30) >> 4) as usize;
          let reg = reg_map[dst];
          let val = read_reg16(reg);
          write_reg16(reg, dec_u16_wrap(val));
        }
        else {
          report_unknown("DEC");
        }
      },
      TableID::IX => {
        if self.len == 0 && self.code == 0x2B {
          // DEX IX
          let val = read_reg16(RegID::IX);
          write_reg16(RegID::IX, dec_u16_wrap(val));
        }
        if self.len == 1 && self.code == 0x35 {
          // DEC (IX + d)
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val = Memory::read8(src_addr);
          let dec_val = dec_u8_wrap(val);
          Memory::write8(src_addr, dec_val);
          update_flags_for_dec(val,dec_val);
        }
        else {
          report_unknown("DEC")
        }
      }
      TableID::IY => {
        if self.len == 0 && self.code == 0x2B {
          // DEX IY
          let val = read_reg16(RegID::IY);
          write_reg16(RegID::IY, dec_u16_wrap(val));
        }
        if self.len == 1 && self.code == 0x35 {
          // DEC (IY + d)
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val = Memory::read8(src_addr);
          let dec_val = dec_u8_wrap(val);
          Memory::write8(src_addr, dec_val);
          update_flags_for_dec(val, dec_val);
        }
        else {
          report_unknown("DEC")
        }
      },
      _ => report_unknown("DEC")
    }
  }

  // PUSH Instruction
  fn push(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code & 0xCF == 0xC5 {
          // PUSH rr
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let src = ((self.code & 0x30) >> 4) as usize;
          let src_reg = reg_map[src];
          let val = read_reg16(src_reg);
          let mut addr = read_reg16(RegID::SP);
          addr = ((addr as i32) - 2) as u16;
          write_reg16(RegID::SP, addr);
          Memory::write16(addr, val);
        }
        else {
          report_unknown("PUSH");
        }
      },
      TableID::IX => {
        if self.code == 0xE5 {
          // PUSH IX
          let val = read_reg16(RegID::IX);
          let mut addr = read_reg16(RegID::SP);
          addr = ((addr as i32) - 2) as u16;
          write_reg16(RegID::SP, addr);
          Memory::write16(addr, val);
        }
        else {
          report_unknown("PUSH");
        }
      }
      TableID::IY => {
        if self.code == 0xE5 {
          // PUSH IY
          let val = read_reg16(RegID::IY);
          let mut addr = read_reg16(RegID::SP);
          addr = ((addr as i32) - 2) as u16;
          write_reg16(RegID::SP, addr);
          Memory::write16(addr, val);
        }
        else {
          report_unknown("PUSH");
        }
      },
      _ => report_unknown("PUSH")
    }
  }

  // POP Instruction
  fn pop(&self) {
    match self.table {
      TableID::MAIN => {
        if self.code & 0xCF == 0xC1 {
          // POP rr
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let rr = ((self.code & 0x30) >> 4) as usize;
          let dst_reg = reg_map[rr];
          let mut addr = read_reg16(RegID::SP);
          let val = Memory::read16(addr);
          write_reg16(dst_reg, val);
          addr = ((addr as u32) + 2) as u16;
          write_reg16(RegID::SP, addr);
        }
        else {
          report_unknown("POP");
        }
      },
      TableID::IX => {
        if self.code == 0xE1 {
          // POP IX
          let mut addr = read_reg16(RegID::SP);
          let val = Memory::read16(addr);
          write_reg16(RegID::IX, val);
          addr = ((addr as u32) + 2) as u16;
          write_reg16(RegID::SP, addr);
        }
        else {
          report_unknown("POP");
        }
      },
      TableID::IY => {
        if self.code == 0xE1 {
          // POP IY
          let mut addr = read_reg16(RegID::SP);
          let val = Memory::read16(addr);
          write_reg16(RegID::IY, val);
          addr = ((addr as u32) + 2) as u16;
          write_reg16(RegID::SP, addr);
        }
        else {
          report_unknown("POP");
        }
      },
      _ => report_unknown("POP")
    }
  }

  // ADD Instruction
  fn add(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 {
          if self.code & 0xF8 == 0x80 {
            // ADD A,r
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            let val1 = read_reg8(RegID::A);
            let val2 = {
              if matches!(src_reg, RegID::HL) {
                let src_addr = read_reg16(RegID::HL);
                Memory::read8(src_addr)
              }
              else {
                read_reg8(src_reg)
              }
            };
            let res = (val1 as u16) + (val2 as u16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_addition8(val1, val2, res);
          }
          else if self.code & 0xCF == 0x09 {
            // ADD HL,rr
            let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
            let rr = (self.code & 0x30) >> 4;
            let src_reg = reg_map[rr as usize];
            let val1 = read_reg16(RegID::HL);
            let val2 = read_reg16(src_reg);
            let res = (val1 as u32) + (val2 as u32);
            write_reg16(RegID::HL, (res & 0xFFFF) as u16);
            update_flags_for_addition16(val1, res);
          }
          else {
            report_unknown("ADD");
          }
        }
        else if self.len == 1 {
          if self.code == 0xC6 {
            // ADD A,n
            let val1 = read_reg8(RegID::A);
            let val2 = ((self.data >> 8) & 0xFF) as u8;
            let res = (val1 as u16) + (val2 as u16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_addition8(val1, val2, res);
          }
          else {
            report_unknown("ADD");
          }
        }
        else {
          report_unknown("ADD");
        }
      },
      TableID::IX => {
        if self.code == 0x86 {
          // ADD A, (IX + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as u16) + (val2 as u16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_addition8(val1, val2, res);
        }
        else if self.code & 0xCF == 0x09 {
          // ADD IX, rr
          let val1 = read_reg16(RegID::IX);
          let reg_map = [RegID::BC, RegID::DE, RegID::IX, RegID::SP];
          let rr = self.code & 0x30;
          let val2 = read_reg16(reg_map[rr as usize]);
          let res = (val1 as u32) + (val2 as u32);
          write_reg16(RegID::IX, (res & 0xFFFF) as u16);
          update_flags_for_addition16(val1, res);
        }
        else {
          report_unknown("ADD");
        }
      }
      TableID::IY => {
        if self.code == 0x86 {
          // ADD A, (IY + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as u16) + (val2 as u16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_addition8(val1, val2, res);
        }
        else if self.code & 0xCF == 0x09 {
          // ADD IY, rr
          let val1 = read_reg16(RegID::IY);
          let reg_map = [RegID::BC, RegID::DE, RegID::IY, RegID::SP];
          let rr = self.code & 0x30;
          let val2 = read_reg16(reg_map[rr as usize]);
          let res = (val1 as u32) + (val2 as u32);
          write_reg16(RegID::IY, (res & 0xFFFF) as u16);
          update_flags_for_addition16(val1, res);
        }
        else {
          report_unknown("ADD");
        }
      },
      _ => report_unknown("ADD")
    }
  }

  // ADC Instruction
  fn adc(&self) {
    let carry: u8 = {
      if is_flag_set(Flag::C) { 1 }
      else { 0 }
    };
    match self.table {
      TableID::MAIN => {
        if self.len == 0 {
          if self.code & 0xF8 == 0x88 {
            // ADC A,r
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            let val1 = read_reg8(RegID::A);
            let val2 = {
              if matches!(src_reg, RegID::HL) {
                let src_addr = read_reg16(RegID::HL);
                Memory::read8(src_addr)
              }
              else {
                read_reg8(src_reg)
              }
            };
            let res = (val1 as u16) + (val2 as u16) + (carry as u16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
          }
          else {
            report_unknown("ADC");
          }
        }
        else if self.len == 1 {
          if self.code == 0xCE {
            // ADC A,n
            let val1 = read_reg8(RegID::A);
            let val2 = ((self.data >> 8) & 0xFF) as u8;
            let res = (val1 as u16) + (val2 as u16) + (carry as u16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
          }
          else {
            report_unknown("ADC");
          }
        }
        else {
          report_unknown("ADC");
        }
      },
      TableID::MISC => {
        if self.code & 0xCF == 0x4A {
          // ADC HL,rr
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let rr = (self.code & 0x30) >> 4;
          let src_reg = reg_map[rr as usize];
          let val1 = read_reg16(RegID::HL);
          let val2 = read_reg16(src_reg);
          let res = (val1 as u32) + (val2 as u32) + (carry as u32);
          write_reg16(RegID::HL, (res & 0xFFFF) as u16);
          update_flags_for_addition_with_carry16(val1, val2, res);
        }
        else {
          report_unknown("ADC");
        }
      },
      TableID::IX => {
      // ADC A, (IX + d)
        if self.code == 0x8E {
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as u16) + (val2 as u16) + (carry as u16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
        }
        else {
          report_unknown("ADC");
        }
      }
      TableID::IY => {
        // ADC A, (IY + d)
        if self.code == 0x8E {
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as u16) + (val2 as u16) + (carry as u16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_addition8(val1, u8_plus_carry_wrap(val2, carry), res);
        }
        else {
          report_unknown("ADC");
        }
      },
      _ => report_unknown("ADC")
    }
  }

  // SUB Instruction
  fn sub(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 {
          if self.code & 0xF8 == 0x90 {
            // SUB A,r
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            let val1 = read_reg8(RegID::A);
            let val2 = {
              if matches!(src_reg, RegID::HL) {
                let src_addr = read_reg16(RegID::HL);
                Memory::read8(src_addr)
              }
              else {
                read_reg8(src_reg)
              }
            };
            let res = (val1 as i16) - (val2 as i16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_subtraction8(val1, val2, res as u16);
          }
          else {
            report_unknown("SUB");
          }
        }
        else if self.len == 1 {
          if self.code == 0xD6 {
            // SUB A,n
            let val1 = read_reg8(RegID::A);
            let val2 = ((self.data >> 8) & 0xFF) as u8;
            let res = (val1 as i16) - (val2 as i16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_subtraction8(val1, val2, res as u16);
          }
          else {
            report_unknown("SUB");
          }
        }
        else {
          report_unknown("SUB");
        }
      },
      TableID::IX => {
        if self.code == 0x96 {
          // SUB A, (IX + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as i16) - (val2 as i16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, val2, res as u16);
        }
        else {
          report_unknown("SUB");
        }
      },
      TableID::IY => {
        if self.code == 0x96 {
          // SUB A, (IY + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as i16) - (val2 as i16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, val2, res as u16);
        }
        else {
          report_unknown("SUB");
        }
      },
      _ => report_unknown("SUB")
    }
  }

  // SBC Instruction
  fn sbc(&self) {
    let carry: u8 = {
      if is_flag_set(Flag::C) { 1 }
      else { 0 }
    };
    match self.table { 
      TableID::MAIN => {
        if self.len == 0 {
          if self.code & 0xF8 == 0x98 {
            // SBC A,r
            let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
            let r = self.code & 0x07;
            let src_reg = reg_map[r as usize];
            let val1 = read_reg8(RegID::A);
            let val2 = {
              if matches!(src_reg, RegID::HL) {
                let src_addr = read_reg16(RegID::HL);
                Memory::read8(src_addr)
              }
              else {
                read_reg8(src_reg)
              }
            };
            let res = (val1 as i16) - (val2 as i16) - (carry as i16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
          }
          else {
            report_unknown("SBC");
          }
        }
        else if self.len == 1 {
          if self.code == 0xDE {
            // SBC A,n
            let val1 = read_reg8(RegID::A);
            let val2 = ((self.data >> 8) & 0xFF) as u8;
            let res = (val1 as i16) - (val2 as i16) - (carry as i16);
            write_reg8(RegID::A, (res & 0xFF) as u8);
            update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
          }
          else {
            report_unknown("SBC");
          }
        }
        else {
          report_unknown("SBC");
        }
      },
      TableID::MISC => {
        if self.code & 0xCF == 0x42 {
          // SDC HL,rr
          let reg_map = [RegID::BC, RegID::DE, RegID::HL, RegID::SP];
          let rr = (self.code & 0x30) >> 4;
          let src_reg = reg_map[rr as usize];
          let val1 = read_reg16(RegID::HL);
          let val2 = read_reg16(src_reg);
          let res = (val1 as i32) - (val2 as i32) - (carry as i32);
          write_reg16(RegID::HL, (res & 0xFFFF) as u16);
          update_flags_for_subtraction_with_carry16(val1, val2, res as u32);
        }
        else {
          report_unknown("ADC");
        }
      },
      TableID::IX => {
        if self.code == 0x9E {
          // SBC A, (IX + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as i16) - (val2 as i16) - (carry as i16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
        }
        else {
          report_unknown("ADC");
        }
      },
      TableID::IY => {
        if self.code == 0x9E {
          // SBC A, (IY + d)
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          let res = (val1 as i16) - (val2 as i16) - (carry as i16);
          write_reg8(RegID::A, (res & 0xFF) as u8);
          update_flags_for_subtraction8(val1, u8_plus_carry_wrap(val2, carry), res as u16);
        }
        else {
          report_unknown("ADC");
        }
      },
      _ => report_unknown("SBC")
    }
  }

  // NEG Instruction
  fn neg(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0x44 {
          let val = read_reg8(RegID::A);
          let res = -(val as i16);
          write_reg8(RegID::A, res as u8);
          update_flags_for_negation8(val, res as u16);
        }
        else {
          report_unknown("NEG");
        }
      },
      _ => report_unknown("NEG")
    }
  }

  // AND Instruction
  fn and(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 && self.code & 0xF8 == 0xA0 {
          // AND r
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8(RegID::A);
          if matches!(src_reg, RegID::HL) {
            // AND (HL)
            let src_addr = read_reg16(RegID::HL);
            let src_val = Memory::read8(src_addr);
            let res = val & src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, true);
          }
            else {
            let src_val = read_reg8(src_reg);
            let res = val & src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, true);
          }
        }
        else if self.len == 1 && self.code == 0xE6 {
          // AND n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8(RegID::A);
          let res = val & src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, true);
        }
        else {
          report_unknown("AND");
        }
      },
      TableID::IX => {
        if self.len == 1 && self.code == 0xA6 {
          // AND IX+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val & src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, true);
        }
        else {
          report_unknown("AND");
        }
      },
      TableID::IY => {
        if self.len == 1 && self.code == 0xA6 {
          // AND IY+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val & src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, true);
        }
        else {
          report_unknown("AND");
        }
      },
      _ => report_unknown("AND")
    }
  }

  // OR Instruction
  fn or(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 && self.code & 0xF8 == 0xB0 {
          // OR r
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8(RegID::A);
          if matches!(src_reg, RegID::HL) {
            // OR (HL)
            let src_addr = read_reg16(RegID::HL);
            let src_val = Memory::read8(src_addr);
            let res = val | src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, false);
          }
          else {
            let src_val = read_reg8(src_reg);
            let res = val | src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, false);
          }
        }
        else if self.len == 1 && self.code == 0xF6 {
          // OR n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8(RegID::A);
          let res = val | src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("OR");
        }
      },
      TableID::IX => {
        if self.len == 1 && self.code == 0xB6 {
          // OR IX+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val | src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("OR");
        }
      }
      TableID::IY => {
        if self.len == 1 && self.code == 0xB6 {
          // OR IY+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val | src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("OR");
        }
      },
      _ => report_unknown("OR")
    }
  }

  // XOR Instruction
  fn xor(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 && self.code & 0xF8 == 0xA8 {
          // XOR r
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val = read_reg8(RegID::A);
          if matches!(src_reg, RegID::HL) {
            // XOR (HL)
            let src_addr = read_reg16(RegID::HL);
            let src_val = Memory::read8(src_addr);
            let res = val ^ src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, false);
          }
          else {
            let src_val = read_reg8(src_reg);
            let res = val ^ src_val;
            write_reg8(RegID::A, res);
            update_flags_for_logical_op(res, false);
          }
        }
        else if self.len == 1 && self.code == 0xEE {
          // XOR n
          let src_val = (self.data & 0xff) as u8;
          let val = read_reg8(RegID::A);
          let res = val ^ src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("XOR");
        }
      }
      TableID::IX => {
        if self.len == 1 && self.code == 0xAE {
          // XOR IX+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val ^ src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("XOR");
        }
      },
      TableID::IY => {
        if self.len == 1 && self.code == 0xAE {
          // XOR IY+d
          let val = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let src_val = Memory::read8(src_addr);
          let res = val ^ src_val;
          write_reg8(RegID::A, res);
          update_flags_for_logical_op(res, false);
        }
        else {
          report_unknown("XOR");
        }
      }
      _ => report_unknown("XOR")
    }
  }

  // CP Instruction
  fn cp(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 && self.code & 0xF8 == 0xB8 {
          // CP r
          let reg_map = [RegID::B, RegID::C, RegID::D, RegID::E, RegID::H, RegID::L, RegID::HL, RegID::A];
          let r = self.code & 0x07;
          let src_reg = reg_map[r as usize];
          let val1 = read_reg8(RegID::A);
          if matches!(src_reg, RegID::HL) {
            // CP (HL)
            let src_addr = read_reg16(RegID::HL);
            let val2 = Memory::read8(src_addr);
            update_flags_for_compare8(val1, val2);
          }
          else {
            let val2 = read_reg8(src_reg);
            update_flags_for_compare8(val1, val2);
          }
        }
        else if self.len == 1 && self.code == 0xFE {
          // CP n
          let val1 = read_reg8(RegID::A);
          let val2 = (self.data & 0xff) as u8;
          update_flags_for_compare8(val1, val2);
        }
        else {
          report_unknown("CP");
        }
      },
      TableID::IX => {
        if self.len == 1 && self.code == 0xBE {
          // CP IX+d
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IX);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          update_flags_for_compare8(val1, val2);
        }
        else {
          report_unknown("CP");
        }
      },
      TableID::IY => {
        if self.len == 1 && self.code == 0xBE {
          // CP IY+d
          let val1 = read_reg8(RegID::A);
          let addr = read_reg16(RegID::IY);
          let src_addr = calc_address_with_offset(addr, self.data as u8);
          let val2 = Memory::read8(src_addr);
          update_flags_for_compare8(val1, val2);
        }
        else {
          report_unknown("CP");
        }
      }
      _ => report_unknown("CP")
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
    match self.table {
      TableID::MAIN => {
        if self.code == 0xC3 {
          // JP nn
          let dst_addr = self.data;
          write_reg16(RegID::PC, dst_addr);
        }
        else if self.code == 0xE9 {
          // JP (HL)
          let hl = read_reg16(RegID::HL);
          let dst_addr = Memory::read16(hl);
          write_reg16(RegID::PC, dst_addr);
        }
        else if self.code & 0xC7 == 0xC2 {
          // JP cc,nn
          let dst_addr = self.data;
          let cc_val = (self.code & 0x38) >> 3;
          let cc = Register::condition_code_for(cc_val);
          let flags = read_reg8(RegID::F);
          let should_jump = Register::check_condition(cc, flags);
          if should_jump {
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
        }
        else {
          report_unknown("JP");
        }
      },
      TableID::IX => {
        if self.code == 0xE9 {
          // JP (IX)
          let hl = read_reg16(RegID::IX);
          let dst_addr = Memory::read16(hl);
          write_reg16(RegID::PC, dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
        else {
          report_unknown("JP");
        }
      },
      TableID::IY => {
        if self.code == 0xE9 {
          // JP (IY)
          let hl = read_reg16(RegID::IY);
          let dst_addr = Memory::read16(hl);
          write_reg16(RegID::PC, dst_addr);
          Cpu::set_acitve_cycles(self.cycles.0);
        }
        else {
          report_unknown("JP");
        }
      },
      _ => report_unknown("JP")
    }
  }

  // JR Instruction
  fn jr(&self) {
    match self.table {
      TableID::MAIN => {
        let addr = Cpu::get_register16(RegID::PC);
        let dst_addr = calc_address_with_offset(addr, self.data as u8);
        if self.code == 0x18 {
          // JR d
          write_reg16(RegID::PC, dst_addr);
        }
        else if self.code == 0x20 {
          // JR NZ
          if !is_flag_set(Flag::Z) {
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
        }
        else if self.code == 0x28 {
          // JR Z
          if is_flag_set(Flag::Z) {
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
        }
        else if self.code == 0x30 {
          // JR NC
          if !is_flag_set(Flag::C) {
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
        }
        else if self.code == 0x38 {
          // JR C
          if is_flag_set(Flag::C) {
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
        }
        else {
          report_unknown("JR");
        }
      },
      _ => report_unknown("JR")
    }
  }

  // CALL Instruction
  fn call(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 2 && self.code == 0xCD { 
          // CALL nn
          let val = read_reg16(RegID::PC);
          let mut addr = read_reg16(RegID::SP);
          addr = ((addr as i32) - 2) as u16;
          write_reg16(RegID::SP, addr);
          Memory::write16(addr, val);
          let dst_addr = self.data;
          write_reg16(RegID::PC, dst_addr);
        }
        else if self.len == 2 && self.code & 0xC7 == 0xC4 {
          // CALL cc,nn
          let cc_val = (self.code & 0x38) >> 3;
          let cc = Register::condition_code_for(cc_val);
          let flags = read_reg8(RegID::F);
          let should_call = Register::check_condition(cc, flags);
          if should_call {
            let val = read_reg16(RegID::PC);
            let mut addr = read_reg16(RegID::SP);
            addr = ((addr as i32) - 2) as u16;
            write_reg16(RegID::SP, addr);
            Memory::write16(addr, val);
            let dst_addr = self.data;
            write_reg16(RegID::PC, dst_addr);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
        }
        else {
          report_unknown("CALL");
        }
      },
      _ => report_unknown("CALL")
    }
  }

  // DJNZ Instruction
  fn djnz(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 1 && self.code == 0x10 {
          let mut b = read_reg8(RegID::B);
          b = dec_u8_wrap(b);
          write_reg8(RegID::B, b);
          if b != 0 {
            let mut pc = read_reg16(RegID::PC);
            pc = calc_address_with_offset(pc, self.data as u8);
            write_reg16(RegID::PC, pc);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
        }
        else {
          report_unknown("DJNZ");
        }
      },
      _ => report_unknown("DJNZ")
      }
  }

  // RET Instruciton
  fn ret(&self) {
    match self.table {
      TableID::MAIN => {
        if self.len == 0 && self.code == 0xC9 {
          // RET
          let sp = read_reg16(RegID::SP);
          let pc = Memory::read16(sp);
          write_reg16(RegID::SP, ((sp as u32) + 2) as u16);
          write_reg16(RegID::PC, pc);
        }
        else if self.code & 0xC7 == 0xC0 {
          // RET cc
          let cc_val = (self.code & 0x38) >> 3;
          let cc = Register::condition_code_for(cc_val);
          let flags = read_reg8(RegID::F);
          let should_return = Register::check_condition(cc, flags);
          if should_return {
            let sp = read_reg16(RegID::SP);
            let pc = Memory::read16(sp);
            write_reg16(RegID::SP, ((sp as u32) + 2) as u16);
            write_reg16(RegID::PC, pc);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
        }
        else {
          report_unknown("RET");
        }
      },
      _ => report_unknown("RET")
    }
  }

  // LDI Instruction
  fn ldi(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xA0 {
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          Memory::write8(de, val);
          write_reg16(RegID::DE, inc_u16_wrap(de));
          write_reg16(RegID::HL, inc_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
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
      },
      _ => report_unknown("LDI")
    }
  }

  // LDIR Instruction
  fn ldir(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xB0 {
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          Memory::write8(de, val);
          write_reg16(RegID::DE, inc_u16_wrap(de));
          write_reg16(RegID::HL, inc_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          if bc != 0x00 {
            let mut pc = read_reg16(RegID::PC);
            pc = calc_address_with_offset(pc, 0xfe);
            write_reg16(RegID::PC, pc);
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
      },
      _ => report_unknown("LDIR")
    }
  }

  // LDD
  fn ldd(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xA8 {
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          Memory::write8(de, val);
          write_reg16(RegID::DE, dec_u16_wrap(de));
          write_reg16(RegID::HL, dec_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
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
      },
      _ => report_unknown("LDD")
    }
  }

  // LDDR Instruction
  fn lddr(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xB8 {
          let de = read_reg16(RegID::DE);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          Memory::write8(de, val);
          write_reg16(RegID::DE, dec_u16_wrap(de));
          write_reg16(RegID::HL, dec_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          if bc != 0x00 {
            let mut pc = read_reg16(RegID::PC);
            pc = calc_address_with_offset(pc, 0xfe);
            write_reg16(RegID::PC, pc);
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
      },
      _ => report_unknown("LDDR")
    }
  }

  // CPI Instruction
  fn cpi(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xA1 {
          let a = read_reg8(RegID::A);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          write_reg16(RegID::HL, inc_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          let res = ((a as i16) - (val as i16)) as u16;
          if res & 0x80 == 0x80 {
            set_flag(Flag::S);
          }
          else {
            clear_flag(Flag::S);
          }
          if res == 0x00 {
            set_flag(Flag::Z);
          }
          else {
            clear_flag(Flag::Z);
          }
          if a & 0x18 == 0x10 && res & 0x18 == 0x08 {
            set_flag(Flag::H);
          }
          else {
            clear_flag(Flag::H);
          }
          if dec_u16_wrap(bc) != 0x0000 {
            set_flag(Flag::PV);
          }
          else {
            clear_flag(Flag::PV);
          }
          set_flag(Flag::N);
        }
        else {
          report_unknown("CPI");
        }
      },
      _ => report_unknown("CPI")
    }
  }

  // CPIR Instruction
  fn cpir(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xB1 {
          let a = read_reg8(RegID::A);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          write_reg16(RegID::HL, inc_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          let res = ((a as i16) - (val as i16)) as u16;
          if bc != 0x00 && res != 0x0000 {
            let mut pc = read_reg16(RegID::PC);
            pc = calc_address_with_offset(pc, 0xfe);
            write_reg16(RegID::PC, pc);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
          if res & 0x80 == 0x80 {
            set_flag(Flag::S);
          }
          else {
            clear_flag(Flag::S);
          }
          if res == 0x00 {
            set_flag(Flag::Z);
          }
          else {
            clear_flag(Flag::Z);
          }
          if a & 0x18 == 0x10 && res & 0x18 == 0x08 {
            set_flag(Flag::H);
          }
          else {
            clear_flag(Flag::H);
          }
          if dec_u16_wrap(bc) != 0x0000 {
            set_flag(Flag::PV);
          }
          else {
            clear_flag(Flag::PV);
          }
          set_flag(Flag::N);
        }
        else {
          report_unknown("CPI");
        }
      },
      _ => report_unknown("CPI")
    }
  }

  // CPD Instruction
  fn cpd(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xA9 {
          let a = read_reg8(RegID::A);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          write_reg16(RegID::HL, dec_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          let res = ((a as i16) - (val as i16)) as u16;
          if res & 0x80 == 0x80 {
            set_flag(Flag::S);
          }
          else {
            clear_flag(Flag::S);
          }
          if res == 0x00 {
            set_flag(Flag::Z);
          }
          else {
            clear_flag(Flag::Z);
          }
          if a & 0x18 == 0x10 && res & 0x18 == 0x08 {
            set_flag(Flag::H);
          }
          else {
            clear_flag(Flag::H);
          }
          if dec_u16_wrap(bc) != 0x0000 {
            set_flag(Flag::PV);
          }
          else {
            clear_flag(Flag::PV);
          }
          set_flag(Flag::N);
        }
        else {
          report_unknown("CPD");
        }
      },
      _ => report_unknown("CPD")
    }
  }

  // CPDR Instruction
  fn cpdr(&self) {
    match self.table {
      TableID::MISC => {
        if self.code == 0xA9 {
          let a = read_reg8(RegID::A);
          let hl = read_reg16(RegID::HL);
          let mut bc = read_reg16(RegID::BC);
          let val = Memory::read8(hl);
          write_reg16(RegID::HL, dec_u16_wrap(hl));
          bc = dec_u16_wrap(bc);
          write_reg16(RegID::BC, bc);
          let res = ((a as i16) - (val as i16)) as u16;
          if bc != 0x00 && res != 0x0000 {
            let mut pc = read_reg16(RegID::PC);
            pc = calc_address_with_offset(pc, 0xfe);
            write_reg16(RegID::PC, pc);
            Cpu::set_acitve_cycles(self.cycles.0);
          }
          else {
            Cpu::set_acitve_cycles(self.cycles.1);
          }
          if res & 0x80 == 0x80 {
            set_flag(Flag::S);
          }
          else {
            clear_flag(Flag::S);
          }
          if res == 0x00 {
            set_flag(Flag::Z);
          }
          else {
            clear_flag(Flag::Z);
          }
          if a & 0x18 == 0x10 && res & 0x18 == 0x08 {
            set_flag(Flag::H);
          }
          else {
            clear_flag(Flag::H);
          }
          if dec_u16_wrap(bc) != 0x0000 {
            set_flag(Flag::PV);
          }
          else {
            clear_flag(Flag::PV);
          }
          set_flag(Flag::N);
        }
        else {
          report_unknown("CPDR");
        }
      }
      _ => report_unknown("CPDR")
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

