use crate::factory::Quad;
use hashbrown::HashMap;
use heapless::String;

pub struct Store<T, S, P, O, G>
where
  T: heapless::ArrayLength<u8>,
  S: heapless::ArrayLength<u8>,
  P: heapless::ArrayLength<u8>,
  O: heapless::ArrayLength<u8>,
  G: heapless::ArrayLength<u8>,
{
  size: u32,
  blank_node_index: u32,
  graphs: HashMap<String<T>, Quad<S, P, O, G>>,

  /// Hidden secret compilation unit. Prevents external manual construction of Store, forcing external users to go through the [`Store::new`] constructor.
  _secret: (),
}

impl<T, S, P, O, G> Store<T, S, P, O, G>
where
  T: heapless::ArrayLength<u8>,
  S: heapless::ArrayLength<u8>,
  P: heapless::ArrayLength<u8>,
  O: heapless::ArrayLength<u8>,
  G: heapless::ArrayLength<u8>,
{
  pub fn new() -> Store<T, S, P, O, G> {
    Store {
      size: 0,
      blank_node_index: 0,
      graphs: HashMap::new(),
      _secret: (),
    }
  }
}
