use std::{sync::mpsc::channel, thread, time::Duration};

pub struct Ula;

pub struct DataBus {
  pub value: u8
}

pub trait Runner: Send + Sync {
  type ReturnType: Send;
  fn run(&self) -> Option<Self::ReturnType>;
}

impl Runner for DataBus {
  type ReturnType = u8;

  fn run(&self) -> Option<Self::ReturnType> {
    thread::sleep(Duration::from_millis(20));
    return Some(self.value);
  }
}

impl Ula {

  pub fn next_vblank<TIn: Runner>(f: &'static TIn) -> impl FnOnce()-> Option<TIn::ReturnType> {
    let (sender, receiver) = channel::<Option<TIn::ReturnType>>();   
    let hand = thread::spawn(move || {
        sender.send(f.run()).unwrap(); 
    });
    let f = move || -> Option<TIn::ReturnType> {
        let res = receiver.recv().unwrap();
        hand.join().unwrap();
        return res;
    };
    return f;
  }
  
}