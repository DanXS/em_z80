
use std::sync::Mutex;
use std::thread;
use core::time::Duration;
use crate::util::*;
use crate::instructions::Instruction;
use crate::opcodes;
use crate::opcodes::Opcode;
use crate::memory::Memory;
use crate::registers::Register;
use crate::registers::Flag;

pub static mut REG : Mutex<Register> = Mutex::new(Register {
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
});

#[inline]
pub fn read_reg8(reg : &str) -> u8 {
  unsafe {
    REG.lock().unwrap().read8(reg)
  }
}

#[inline]
pub fn read_reg16(reg : &str) -> u16 {
  unsafe {
    REG.lock().unwrap().read16(reg)
  }
}

#[inline]
pub fn write_reg8(reg : &str, val: u8) {
  unsafe {
    REG.lock().unwrap().write8(reg, val);
  }
}

#[inline]
pub fn write_reg16(reg : &str, val: u16) {
  unsafe {
    REG.lock().unwrap().write16(reg, val);
  }
}

#[inline]
pub fn set_flag(flag: Flag) {
  unsafe {
    REG.lock().unwrap().set_flag(flag);
  }
}

#[inline]
pub fn clear_flag(flag: Flag) {
  unsafe {
    REG.lock().unwrap().clear_flag(flag);
  }
}

#[inline]
pub fn is_flag_set(flag: Flag) -> bool {
  unsafe {
    REG.lock().unwrap().is_flag_set(flag)
  }
}

#[inline]
pub fn update_flags_for_inc(val : u8, inc_val : u8) {
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
}

#[inline]
pub fn update_flags_for_dec(val : u8, dec_val : u8) {
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
  set_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_addition8(val1 : u8, val2: u8, res: u16) {
  if res & 0x80 == 0x80  {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S)
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if ((val1 & 0x0F) + (val2 & 0x0F)) & 0x10 == 0x10 {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if res & 0x100 == 0x100 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if (val1 & 0x80 == 0x80) && (val2 & 0x80 == 0x80) && (res & 0x80 == 0x00) ||
     (val1 & 0x80 == 0x00) && (val2 & 0x80 == 0x00) && (res & 0x80 == 0x80) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  clear_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_subtraction8(val1 : u8, val2: u8, res: u16) {
  if res & 0x80 == 0x80  {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S)
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if ((val1 & 0x1F) - (val2 & 0x1F)) & 0x80 == 0x80 {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if res & 0x100 == 0x100 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if (val1 & 0x80 == 0x80) && (val2 & 0x80 == 0x00) && (res & 0x80 == 0x00) ||
     (val1 & 0x80 == 0x00) && (val2 & 0x80 == 0x80) && (res & 0x80 == 0x80) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  set_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_addition16(val1: u16, res: u32) {
  if res & 0x10000 == 0x10000 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if (val1 & 0x0C00 == 0x0400) && (res & 0x0800 == 0x800) {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  clear_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_addition_with_carry16(val1: u16, val2: u16, res: u32) {
  if res & 0x8000 == 0x8000 {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S);
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if res & 0x10000 == 0x10000 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if (val1 & 0x0C00 == 0x0800) && (res & 0x0400 == 0x0400) {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if (val1 & 0x8000 == 0x8000) && (val2 & 0x8000 == 0x8000) && (res & 0x8000 == 0x0000) ||
     (val1 & 0x8000 == 0x0000) && (val2 & 0x8000 == 0x0000) && (res & 0x8000 == 0x8000) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  clear_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_subtraction_with_carry16(val1 : u16, val2: u16, res : u32) {
  if res & 0x8000 == 0x8000 {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S);
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if res & 0x10000 == 0x10000 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if (val1 & 0x0C00 == 0x0800) && (res & 0x0400 == 0x0400) {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if (val1 & 0x8000 == 0x8000) && (val2 & 0x8000 == 0x0000) && (res & 0x8000 == 0x0000) ||
     (val1 & 0x8000 == 0x0000) && (val2 & 0x8000 == 0x8000) && (res & 0x8000 == 0x8000) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  set_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_negation8(val: u8, res: u16) {
  if res & 0x80 == 0x80  {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S)
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if val & 0x18 == 0x10 && res & 0x18 == 0x08 {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if val != 0x00 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  if val == 0x80 {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  set_flag(Flag::N);
}

#[inline]
pub fn get_parity(val: u8) -> bool {
  let mut parity = true;
  for i in 0..7 {
    if (val >> i) & 0x01 == 0x01 {
      parity = !parity;
    }
  }
  parity
}

#[inline]
pub fn update_flags_for_logical_op(res: u8, set_h: bool) {
  if res & 0x80 == 0x80  {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S)
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if get_parity(res) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  if set_h {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  clear_flag(Flag::C);
  clear_flag(Flag::N);
}

#[inline]
pub fn update_flags_for_compare8(val1: u8, val2: u8) {
  let res = (val1 as i16) - (val2 as i16);
  if res & 0x80 == 0x80  {
    set_flag(Flag::S);
  }
  else {
    clear_flag(Flag::S)
  }
  if res == 0 {
    set_flag(Flag::Z);
  }
  else {
    clear_flag(Flag::Z);
  }
  if (val1 & 0x80 == 0x80) && (val2 & 0x80 == 0x00) && (res & 0x80 == 0x00) ||
     (val1 & 0x80 == 0x00) && (val2 & 0x80 == 0x80) && (res & 0x80 == 0x80) {
    set_flag(Flag::PV);
  }
  else {
    clear_flag(Flag::PV);
  }
  if val1 & 0x18 == 0x10 && res & 0x18 == 0x08  {
    set_flag(Flag::H);
  }
  else {
    clear_flag(Flag::H);
  }
  if res & 0x0100 == 0x0100 {
    set_flag(Flag::C);
  }
  else {
    clear_flag(Flag::C);
  }
  set_flag(Flag::N);
}

struct CpuState {
  pub cycle_time: u64,
  pub active_cycles: u8,
  pub breakpoints: Vec<u16>,
  pub breakpoints_enabled: bool,
  pub is_running: bool
}

static mut CPU_STATE : Mutex<CpuState> = Mutex::new(CpuState { 
  cycle_time: 286,
  active_cycles: 0,
  breakpoints: Vec::new(),
  breakpoints_enabled: false,
  is_running: false
});

pub struct Cpu;

impl Cpu {

  pub fn get_pc() -> u16 {
    read_reg16("PC")
  }

  pub fn set_pc(addr: u16) {
    write_reg16("PC", addr);
  }

  pub fn get_register8(reg: &str) -> u8 {
    read_reg8(reg)
  }

  pub fn set_register8(reg: &str, val: u8) {
    write_reg8(reg, val);
  }

  pub fn get_register16(reg: &str) -> u16 {
    read_reg16(reg)
  }

  pub fn set_register16(reg: &str, val: u16) {
    write_reg16(reg, val);
  }

  pub fn set_status_flag(flag: Flag) {
    set_flag(flag);
  }
  
  pub fn clear_status_flag(flag: Flag) {
    clear_flag(flag);
  }
  
  pub fn get_status_flag(flag: Flag) -> bool {
    is_flag_set(flag)
  }

  pub fn set_cpu_frequency(mhz: f32) {
    if mhz <= 0.0f32 {
      panic!("Attempt to set cpu frequency to zero or negative value, please pass a positive value only!");
    }
    else {
      let ns = (1.0f32/mhz*1e3) as u64;
      println!("CPU frequency {}mhz, cycle time {}ns", mhz, ns);
      Cpu::set_cycle_time(ns);
    }
  }

  pub fn get_cycle_time() -> u64 {
    unsafe {
      CPU_STATE.lock().unwrap().cycle_time
    }
  }

  pub fn set_cycle_time(ns: u64) {
    unsafe {
      CPU_STATE.lock().unwrap().cycle_time = ns;
    }
  }

  pub fn get_acitve_cycles() -> u8 {
    unsafe {
      CPU_STATE.lock().unwrap().active_cycles
    }
  }

  pub fn set_acitve_cycles(cycles: u8) {
    unsafe {
      CPU_STATE.lock().unwrap().active_cycles = cycles;
    }
  }

  pub fn get_register_view_string() -> String {
    unsafe { REG.lock().unwrap().to_string() }
  }

  pub fn toggle_breakpoint(addr: u16) {
    unsafe {
      let mut bps = CPU_STATE.lock().unwrap().breakpoints.clone();
      match bps.binary_search(&addr) {
        Ok(pos) => {
            println!("Removing breakpoint {} at position {}", addr, pos);
            bps.remove(pos);
            CPU_STATE.lock().unwrap().breakpoints = bps;
        },
        Err(pos) => {
            println!("Adding breakpoint {} at position {}", addr, pos);
            bps.insert(pos, addr.clone());
            CPU_STATE.lock().unwrap().breakpoints = bps;
        }
      }
    }
  }

  pub fn update_breakpoints_enabled(enabled : bool) {
    unsafe {
      CPU_STATE.lock().unwrap().breakpoints_enabled = enabled;
    }
  }

  pub fn has_breakpoint(addr: u16) -> bool {
    unsafe{
      CPU_STATE.lock().unwrap().breakpoints.contains(&addr)
    }
  }

  pub fn disassemble(addr: u16) -> (String, u8) {
    // Fetch the opcode from the opcode tables
    let (opcode, data, n) = Self::fetch(addr);
    let inst = opcode.instruction;
    // If the operand is two bytes long replace the 'nn' of the instruction by the
    // two data bytes
    if n == 2 {
      if inst.contains("nn") {
        let s = format!("{:02X?}{:02X?}H", data[1], data[0]);
        (inst.replace("nn", s.as_str()), opcode.byte_count)
      }
      else {
        let s0 = format!("{:02X?}H", data[0]);
        let s1 = format!("{:02X?}H", data[1]);
        (inst.replace("d", s0.as_str()).replace("n", s1.as_str()), opcode.byte_count)
      }
    }
    else if n == 1 {
        // Otherwise if the instruction has one byte of data
        // replace the 'n' or 'd' by the single byte of data
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
      // Read the first opcode byte
      let code = Memory::read8(address);
      let mut opcode = &opcodes::MAIN_OPCODES[code as usize];
      // Increment address
      address = inc_u16_wrap(address);
      let mut inst_len : usize = 1;
      // Check for alternate multibyte opcode
      if opcode.instruction == "MISC" {
        // It's a two byte opcode from MISC table
        let code = Memory::read8(address);
        opcode = &opcodes::MISC_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
      }
      else if opcode.instruction == "BIT" {
        // It's a two byte opcode from BIT table
        let code = Memory::read8(address);
        opcode = &opcodes::BIT_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
      }
      else if opcode.instruction == "IX" {
        // It's a two byte opcode from IX table
        let code = Memory::read8(address);
        opcode = &opcodes::IX_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
        if opcode.instruction == "IX_BIT" {
          // It's a three byte opcode from IX_BIT table
          let code = Memory::read8(address);
          opcode = &opcodes::IX_BIT_OPCODES[code as usize];
          address = inc_u16_wrap(address);
          inst_len = 3;
        }
      }
      else if opcode.instruction == "IY" {
        // It's a two byte opcode from IY table
        let code = Memory::read8(address);
        opcode = &opcodes::IY_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
        if opcode.instruction == "IY_BIT" {
          // It's a three byte opcode from IY_BIT table
          let code = Memory::read8(address);
          opcode = &opcodes::IY_BIT_OPCODES[code as usize];
          address = inc_u16_wrap(address);
          inst_len = 3;
        }
      }
      // Read the remaining bytes of data for the operand's
      let mut data: [u8;2] = [0,0];
      let n = (opcode.byte_count as usize) - inst_len;
      for i in 0..n {
        data[i] = Memory::read8(address);
        address = inc_u16_wrap(address);
      }
      (opcode, data, n)
  }

  pub fn step() {
    // Fetch the instruction
    let (opcode, data, n) = Self::fetch(Self::get_pc());
    // Increment the program counter
    write_reg16("PC", (((Self::get_pc() as u32) + (opcode.byte_count as u32)) & 0xFFFF) as u16);
    // Run the instruction
    Self::run_inst(opcode.func, opcode, data, n, opcode.table, opcode.cycles);
  }

  fn run_inst(f: fn(&Instruction), opcode: &Opcode, data: [u8;2], len: usize, table: &str, cycles: (u8,u8)) {
    // The minimum number of cycles unless changed by the instruction itself
    // For example, branching may take more cycles (cycles.0)
    Self::set_acitve_cycles(cycles.1);
    // Construct the instruction
    let inst = Instruction{code: opcode.code, 
      data: ((data[1] as u16) << 8) | (data[0] as u16), 
      len: len, table: String::from(table),
      cycles: cycles};
    // Run the instruction
    f(&inst);
    // Simulate the time it takes for the instruciton to run
    let delay_ns = Self::get_cycle_time()*(Self::get_acitve_cycles() as u64);
    println!("Cycles to execute: {}, delay {}ns", Self::get_acitve_cycles(), delay_ns);
    thread::sleep(Duration::from_nanos(delay_ns));
  }

  pub fn run() {
    unsafe { CPU_STATE.lock().unwrap().is_running = true };
    while unsafe { CPU_STATE.lock().unwrap().is_running } {
      let (text, _) = Self::disassemble(Self::get_pc());
      println!("Step:\n{:04X?}: {} ", Self::get_pc(), text);
      Self::step();
      if unsafe { CPU_STATE.lock().unwrap().breakpoints_enabled } {
        if unsafe { CPU_STATE.lock().unwrap().breakpoints.contains(&Self::get_pc()) } {
          println!("Hit breakpoint at {:40X?}", &Self::get_pc());
          break;
        }
      }
    }
    unsafe { CPU_STATE.lock().unwrap().is_running = false };
  }

  pub fn stop() {
    unsafe { CPU_STATE.lock().unwrap().is_running = false };
  }

}





