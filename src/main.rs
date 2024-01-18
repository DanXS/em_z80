mod cpu;
mod registers;
mod memory;
mod opcodes;

use crate::cpu::Cpu;
use crate::cpu::REG;
use crate::memory::Memory;

fn main() {
    let rom_file_result = Memory::load_rom("./spectrum48k.rom");
    println!("Memory:\n{}", Memory);
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    let mut pc = Cpu::get_pc();
    while u32::from(pc) <= memory::ROM_SIZE {
        let (text, _) = Cpu::disassemble(pc);
        println!("{:04X?}\t{}", pc, text);
        Cpu::step();
        pc = Cpu::get_pc();
        unsafe {
            println!("Registers:\n{}", REG);
        }
        println!("---------------------------------------");
    }

}


