
extern crate em_z80_lib;

use em_z80_lib::*;
/*
fn main() {
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    println!("Memory:\n{}", get_memory_ref());
    let mut pc = get_pc();
    while u32::from(pc) <= get_rom_size() {
        let (text, _) = disassemble_addr(pc);
        println!("{:04X?}\t{}", pc, text);
        step();
        pc = get_pc();
        println!("Registers:\n{}", get_reg_ref());
        println!("---------------------------------------");
    }
}
*/

use slint::{SharedString, ModelRc, VecModel};
use std::rc::Rc;

fn main() {
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    let main_window = MainWindow::new().unwrap();
    init_memory_view(&main_window);
    init_register_view(&main_window);
    init_disassembly_view(&main_window);
    main_window.run().unwrap();
}

fn init_memory_view(main_window: &MainWindow) {
    let mem_str = get_memory_view_string();
    let str_vec : Vec<&str> = mem_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let memory_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let memory_view_model_rc = ModelRc::from(memory_view_model.clone());
    main_window.set_memory_view_data(memory_view_model_rc);
}

fn init_register_view(main_window: &MainWindow) {
    let reg_str = get_register_view_string();
    let str_vec : Vec<&str> = reg_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let register_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let register_view_model_rc = ModelRc::from(register_view_model.clone());
    main_window.set_register_view_data(register_view_model_rc);
}

fn init_disassembly_view(main_window: &MainWindow) {
    let mut addr = get_pc();
    let mut str_vec : Vec<String> = Vec::new();
    for _ in 0..20 {
        let (text, bytes) = disassemble_addr(addr);
        str_vec.push(format!("{:04X?}:    {}", addr, text));
        if addr as u32 == get_mem_size()-1 {
            break;
        }
        else {
            addr = addr + (bytes as u16);
        }
    }
    let vec_model : Vec<SharedString> = (str_vec.clone()).iter().map(|x| x.into()).collect();
    let disassembly_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let disassembly_view_model_rc = ModelRc::from(disassembly_view_model.clone());
    main_window.set_disassembly_view_data(disassembly_view_model_rc);
}

slint::slint! {

    import { Button, ListView, HorizontalBox, ScrollView } from "std-widgets.slint";
    import "./fonts/liberation-mono/LiberationMono-Regular.ttf";
    import "./fonts/liberation-mono/LiberationMono-Bold.ttf";
    
    component DisplayView inherits Rectangle {
        width: 62.5%;
        height: self.width * (9.0/16.0);
        background: #CCCCCC;
    }

    component MemoryView inherits Rectangle {
        in property <[string]> data;
        width: 62.5%;
        vertical-stretch: 1;
        background: #6666aa;
        VerticalLayout {
            padding: 2px;
            HorizontalBox {
                Text {
                    color: #eeeeee;
                    font-size: 11pt;
                    font-weight: 700;
                    text: "Memory";
                }
            }
            Rectangle {
                background: #eeeeee;
                width: 100%;
                vertical-stretch: 1;
                ListView {
                    for txt in data
                    : Rectangle {
                        height: 30px;
                        width: 630px;
                        Text {
                            x: 0;
                            text: txt;
                            color: #000000;
                            font-weight: 500;
                            font-size: 11pt;
                        }
                    }
                }
            }
        }
    }

    component RegisterView inherits Rectangle {
        in property <[string]> data;
        width: 37.5%;
        height: 50%;
        background: #6666aa;
        VerticalLayout {
            padding:2px;
            HorizontalBox {
                Text {
                    color: #eeeeee;
                    font-size: 11pt;
                    font-weight: 700;
                    text: "Registers";
                }
            }
            Rectangle {
                background: #eeeeee;
                width: 100%;
                vertical-stretch: 1;
                ListView {
                    width: 100%;       
                    for txt in data
                    : Rectangle {
                        height: 30px;
                        width: parent.width;
                        Text {
                            x: 0;
                            text: txt;
                            color: #000000;
                            font-weight: 500;
                            font-size: 11pt;
                        }
                    }
                }
            }
        }
    }

    component DisassemblyView inherits Rectangle {
        in property <[string]> data;
        width: 37.5%;
        background: #6666aa;
        VerticalLayout {
            padding:2px;
            HorizontalBox {
                Text {
                    color: #eeeeee;
                    font-size: 11pt;
                    font-weight: 700;
                    text: "Disassembly";
                }
            }
            Rectangle {
                background: #eeeeee;
                width: 100%;
                vertical-stretch: 1;
                ListView {
                    width: 100%;       
                    for txt in data
                    : Rectangle {
                        height: 30px;
                        background: #eeeeee;
                        width: parent.width;
                        Text {
                            x: 0;
                            text: txt;
                            color: #000000;
                            font-weight: 500;
                            font-size: 11pt;
                        }
                    }
                }
            }
        }
    }

    export component MainWindow inherits Window {
        in property <[string]> memory_view_data;
        in property <[string]> register_view_data;
        in property <[string]> disassembly_view_data;
        default-font-family: "Liberation Mono";
        preferred-width: 1024px;
        min-width: 640px;
        preferred-height: 576px;
        min-height: 480px;
        title: "Z80 Emulator/Disassembler";
        HorizontalLayout {
            VerticalLayout {
                DisplayView {}
                MemoryView { data: memory_view_data; }
            }
            VerticalLayout {
                RegisterView { data: register_view_data; }
                DisassemblyView { data: disassembly_view_data; }
            }
        }
    }
}

