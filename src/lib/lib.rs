mod cpu;
mod registers;
mod memory;
mod opcodes;

extern crate core;

use std::io;
use registers::Register;

use crate::cpu::Cpu;
use crate::cpu::REG;
use crate::memory::Memory;

// Get the size of the memory used for the emulation
pub fn get_mem_size() -> u32 {
  memory::MEM_SIZE
}

// Get the size of the rom area of memory
pub fn get_rom_size() -> u32 {
  memory::ROM_SIZE
}

// Set the program counter to a particular address
pub fn set_pc(addr: u16) {
  Cpu::set_pc(addr);
}

// Get the program counter's current address
pub fn get_pc() -> u16 {
  Cpu::get_pc()
}

// Set a register to a specified value
pub fn set_register16(reg: &str, val: u16) {
  Cpu::set_register16(reg, val);
}

// Get a register value
pub fn get_register16(reg: &str) -> u16 {
  Cpu::get_register16(reg)
}

// Load a rom image file into the rom area of memory
pub fn load_rom(filename: &str) -> io::Result<()> {
  Memory::load_rom(filename)
}

// Get a reference to the memory
pub fn get_memory_ref() -> &'static Memory {
  &Memory
}

// Dissassmble a given address, returns the dissabled string and the number of bytes
// used to encode the instruction
pub fn disassemble_addr(addr: u16) -> (String, u8) {
  let result = Cpu::disassemble(addr);
  return result;
}

// Step a single instruction at the current address given by the program counter
pub fn step() {
  Cpu::step();
}

// Get a reference to the registers
pub fn get_reg_ref() -> &'static Register {
  unsafe {&REG}
}

// Get a string representing entire memory view
pub fn get_memory_view_string() -> String {
  Memory.to_string()
}

// Get a string representing registers
pub fn get_register_view_string() -> String {
  unsafe {REG.to_string()}
}

