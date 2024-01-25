
extern crate em_z80_lib;

use em_z80_lib::*;

use std::u16;
use std::rc::Rc;
use slint::{SharedString, ModelRc, VecModel};

slint::slint! {

    import { Button, ListView, HorizontalBox, ScrollView, LineEdit, ComboBox } from "std-widgets.slint";
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
        callback set_register(string, string);
        out property <string> reg_str;
        out property <string> val_str;
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
                ComboBox {
                    width: 60px;
                    model: ["", "AF", "AF'", "BC", "BC'", "DE", "DE'", "HL", "HL'", "IX", "IY", "SP", "PC", "IR", "WZ"];
                    current-value: "";
                    selected(reg) => {
                        root.reg_str = reg;
                    }
                }
                LineEdit {
                    width: 60px;
                    placeholder-text: "HEX";
                    enabled: {root.reg_str != ""};
                    edited(val) => {
                        root.val_str = val;
                    }
                    accepted(val) => {
                        root.val_str = val;
                        root.set_register(root.reg_str, root.val_str);
                    }
                }
                Button {
                    width: 60px;
                    text: "Set";
                    enabled: { (root.reg_str != "") };
                    clicked => {
                        root.set_register(root.reg_str, root.val_str);
                    }
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
        in-out property <bool> is_running;
        callback step;
        callback start_stop;
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
                Button {
                    text : "Step";
                    enabled: !is_running; 
                    clicked => {
                        root.step();
                    }
                }
                Button {
                    text: is_running ? "Stop" : "Run";
                    clicked => {
                        root.start_stop();
                    }
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
                        background: {(txt == data[0]) ? #ffAA88 : #eeeeee};
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
        in-out property <bool> is_running;
        callback step;
        callback start_stop;
        callback set_register(string, string);
        default-font-family: "Liberation Mono";
        preferred-width: 1024px;
        min-width: 640px;
        preferred-height: 576px;
        min-height: 480px;
        title: "Z80 Emulator/Disassembler";
        HorizontalLayout {
            VerticalLayout {
                DisplayView {}
                MemoryView  {
                    data: memory_view_data;
                }
            }
            VerticalLayout {
                RegisterView {
                    data: register_view_data;
                    set_register(reg, val) => {
                        root.set_register(reg, val);
                    }
                }
                DisassemblyView {
                    data: disassembly_view_data;
                    is_running: is_running;
                    step => {
                        root.step();
                    }
                    start_stop => {
                        root.start_stop();
                    }
                }
            }
        }
    }
}


fn main() {
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    let main_window = MainWindow::new().unwrap();
    build_memory_view(&main_window);
    build_register_view(&main_window);
    build_disassembly_view(&main_window);
    main_window.set_is_running(false);
    let weak_window = main_window.as_weak();
    main_window.on_step(move || {
        let (text, _) = disassemble_addr(get_pc());
        println!("Step: {} ", text);
        step();
        build_disassembly_view(&(weak_window.unwrap())); 
        build_register_view(&(weak_window.unwrap()));
        build_memory_view(&(weak_window.unwrap()));
    });
    let weak_window = main_window.as_weak();
    main_window.on_start_stop(move || {
        let is_running = weak_window.unwrap().get_is_running();
        if !is_running {
            println!("Start");
            weak_window.unwrap().set_is_running(!is_running);
        }
        else {
            println!("Stop");
            weak_window.unwrap().set_is_running(!is_running);
        }
    });
    let weak_window = main_window.as_weak();
    main_window.on_set_register(move |reg: SharedString, val: SharedString| {
        let reg_str = reg.as_str();
        let val_str = val.as_str();
        match u16::from_str_radix(val_str, 16) {
            Ok(res) => {
                set_register16(reg_str, res);
                build_register_view(&(weak_window.unwrap()));
                if reg_str == "PC" {
                    build_disassembly_view(&(weak_window.unwrap())); 
                }
            },
            Err(e) => println!("{}", e),
        };
    });
    main_window.run().unwrap();
}

fn build_memory_view(main_window: &MainWindow) {
    let mem_str = get_memory_view_string();
    let str_vec : Vec<&str> = mem_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let memory_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let memory_view_model_rc = ModelRc::from(memory_view_model.clone());
    main_window.set_memory_view_data(memory_view_model_rc);
}

fn build_register_view(main_window: &MainWindow) {
    let reg_str = get_register_view_string();
    let str_vec : Vec<&str> = reg_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let register_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let register_view_model_rc = ModelRc::from(register_view_model.clone());
    main_window.set_register_view_data(register_view_model_rc);
}

fn build_disassembly_view(main_window: &MainWindow) {
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
