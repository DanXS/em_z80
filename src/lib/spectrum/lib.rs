mod ula;

use ula::DataBus;
use ula::Ula;

pub fn next_vblank() -> u8 {
  let fut = Ula::next_vblank(&DataBus{value: 0x38});
  fut().unwrap()
}