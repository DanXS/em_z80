
#[inline(always)]
pub fn inc_u8_wrap(val: u8) -> u8 {
  (((val as u16) + 1) & 0xFF) as u8
}

#[inline(always)]
pub fn inc_u16_wrap(val: u16) -> u16 {
  (((val as u32) + 1) & 0xFFFF) as u16
}

#[inline(always)]
pub fn dec_u8_wrap(val: u8) -> u8 {
  (((val as i16) - 1) & 0xFF) as u8
}

#[inline(always)]
pub fn dec_u16_wrap(val: u16) -> u16 {
  (((val as i32) - 1) & 0xFFFF) as u16
}

#[inline(always)]
pub fn calc_address_with_offset(addr: u16, d: u8) -> u16 {
  let offset = (d as i8) as i32;
  (((addr as u32) as i32) + offset) as u16
}

#[inline(always)]
pub fn u8_plus_carry_wrap(val: u8, carry: u8) -> u8 {
  if carry == 0 {
    val
  }
  else {
    inc_u8_wrap(val)
  }
}

#[inline(always)]
pub fn get_parity(val: u8) -> bool {
  let count = [0,1,2,3,4,5,6,7].into_iter()
        .fold(0, |a, b| a + ((val >> b) & 0x01));
  count % 2 == 0
}

#[inline]
pub fn report_unknown(opcode_str: &str) {
  // commented out for now to reduce noise in console
  //println!("Unknown {} instruction!", opcode_str);
}

#[inline]
pub fn report_not_supported(opcode_str: &str) {
  println!("{} instruction is not supported on the z80 - z180 only!", opcode_str);
}