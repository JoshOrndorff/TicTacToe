//! A runtime module to play censorship-resistant tic-tac-toe games

#![cfg_attr(not(feature = "std"), no_std)]

use frame_support::{
	decl_event, decl_module, decl_storage, decl_error,
	dispatch::DispatchResult,
	ensure,
};
use frame_system::ensure_signed;
use parity_scale_codec::{ Encode, Decode };
use sp_std::{vec, vec::{ Vec }};
//use core::convert::TryInto;

/// The module's configuration trait.
pub trait Trait: frame_system::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as frame_system::Trait>::Event>;
}

type GameId = u64;
type CellIndex = u8;


#[derive(Encode, Decode, Eq, PartialEq, Clone, Debug)]
pub enum Line {
	Column(u8),
	Row(u8),
	Downhill,
	Uphill,
}

// This module's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as TicTacToe {
		// The next game index
		NextId get(fn next_id): GameId;

		// The actual cells of each each game.
		Board get(fn board): double_map hasher(blake2_128_concat) GameId, hasher(blake2_128_concat) CellIndex => Option<T::AccountId>;

		// Who is playing in each game
		// This is the canonical way to determine whether a game exists.
		Players get(fn players): map hasher(blake2_128_concat) GameId => Vec<T::AccountId>;

		// Which turn it is in the given game
		Turn get(fn turn): map hasher(blake2_128_concat) GameId => u64;

		// Temporary storage item to keep track of who won each game
		// Ideally the ultimate "result" from a win will be the event
		// or possibly a payout in terms of staked/wagered tokens
		Winner get(fn winner): map hasher(blake2_128_concat) GameId => Option<T::AccountId>
	}
}

decl_error! {
	pub enum Error for Module<T: Trait> {
		/// You've tried to take a turn in a game that doesn't exist
		NoSuchGame,
		/// It is not your turn to make a move (You may not be in this game)
		NotYourTurn,
		/// The cell you've tried to take is already taken
		CellTaken
	}
}

decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Initializing events
		// this is needed only if you are using events in your module
		fn deposit_event() = default;

		/// Create a Game
		#[weight = 10_000]
		pub fn create_game(origin, opponent: T::AccountId) -> DispatchResult {
			// You can only create games that you will play in
			let challenger = ensure_signed(origin)?;

			// Confirm challenger and opponent are different people
			//ensure!(challenger != opponent, "No playing with yourself");

			// Get the next game id, and update counter in storage
			let game = Self::next_id();
			NextId::put(game.wrapping_add(1));

			// Store the players
			<Players<T>>::insert(game, vec![challenger.clone(), opponent.clone()]);

			// Emit the event
			Self::deposit_event(RawEvent::NewGame(game, challenger, opponent));
			Ok(())
		}

		/// Take a normal non-winning turn
		#[weight = 10_000]
		pub fn take_normal_turn(origin, game: GameId, cell: CellIndex) -> DispatchResult {
			let caller = ensure_signed(origin)?;

			Self::take_turn(&caller, game, cell)
		}

		/// Take the final turn of the game and claim the win
		#[weight = 10_000]
		pub fn take_winning_turn(origin, game: GameId, cell: CellIndex, location: Line) -> DispatchResult {
			let caller = ensure_signed(origin)?;

			let turn_result = Self::take_turn(&caller, game, cell);
			match turn_result {
				// Is there some map_err or something for this?
				Err(e) => Err(e),
				Ok(()) => {
					let actually_won = match location {
						Line::Column(n) => Self::check_vertical(game, &caller, n),
						Line::Row(n) => Self::check_horizontal(game, &caller, n),
						Line::Uphill => Self::check_uphill(game, &caller),
						Line::Downhill => Self::check_downhill(game, &caller),
					};

					// If they actually won, emit the event and clean up.
					// Otherwise, just consume their fee.
					if actually_won {
						// Winner field for debugging only. Remove when working
						<Winner<T>>::insert(game, caller.clone());

						Self::deposit_event(RawEvent::Win(game, caller));

						<Board<T>>::remove_prefix(&game);
						<Players<T>>::remove(game);
						Turn::remove(game);
					}

					Ok(())
				}
			}
		}

		//TODO Way to clean up draws and abandoned games
		// fn abort game
	}
}

impl<T: Trait> Module<T> {
	fn take_turn(caller: &T::AccountId, game: GameId, cell: CellIndex) -> DispatchResult {
		// Verify the game id
		ensure!(<Players<T>>::contains_key(game), Error::<T>::NoSuchGame);

		// Verify the palyer
		let current_turn = Turn::get(game);
		let player_index = (current_turn % 2) as usize;
		let player = <Players<T>>::get(game)[player_index].clone();
		ensure!(caller == &player, Error::<T>::NotYourTurn);

		// Verify the cell
		ensure!(!<Board<T>>::contains_key(&game, &cell), Error::<T>::CellTaken);

		// Write to the cell
		<Board<T>>::insert(&game, &cell, caller);

		// Update the turn counter
		// Wrapping add is safe atm because board will fill before turn
		// overflows. Revisit this when (if) board size is customizable.
		Turn::insert(game, current_turn.wrapping_add(1));

		// Emit the event
		Self::deposit_event(RawEvent::TurnTaken(game, caller.clone(), cell));

		Ok(())
	}

	fn check_vertical(game: GameId, winner: &T::AccountId, col: u8) -> bool {
		let size = 3;
		let cells = (0..size).map(|row| row * size + col);
		Self::player_occupies_all_cells(game, winner, cells)
	}

	fn check_horizontal(game: GameId, winner: &T::AccountId, row: u8) -> bool {
		let size = 3;
		let cells = (0..size).map(|col| row * size + col);
		Self::player_occupies_all_cells(game, winner, cells)
	}

	fn check_uphill(game: GameId, winner: &T::AccountId) -> bool {
		let size = 3;
		let cells = (0..size).map(|i| (size - 1) * (i + 1));
		Self::player_occupies_all_cells(game, winner, cells)
	}

	fn check_downhill(game: GameId, winner: &T::AccountId) -> bool {
		let size = 3;
		let cells = (0..size).map(|i| i * (size + 1));
		Self::player_occupies_all_cells(game, winner, cells)
	}

	fn player_occupies_all_cells(game: GameId, player: &T::AccountId, cells: impl Iterator<Item=CellIndex>) -> bool {
		for cell in cells {
			match <Board<T>>::get(&game, &cell) {
				None => { return false; },
				Some(p) => {
					if &p != player {
						return false;
					}
				},
			}
		}
		true
	}
}

decl_event!(
	pub enum Event<T> where AccountId = <T as frame_system::Trait>::AccountId {
		NewGame(GameId, AccountId, AccountId),
		TurnTaken(GameId, AccountId, CellIndex),
		Win(GameId, AccountId),
		Draw(GameId),
	}
);

/// tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use sp_core::H256;
	use frame_support::{impl_outer_origin, assert_ok, parameter_types, assert_noop, impl_outer_event};
	use sp_runtime::{traits::{BlakeTwo256, IdentityLookup}, testing::Header, Perbill};
	use sp_io::TestExternalities;

	#[derive(Clone, PartialEq, Eq, Debug)]
	pub struct TestRuntime;

	impl_outer_origin! {
		pub enum Origin for TestRuntime {}
	}

	parameter_types! {
		pub const BlockHashCount: u64 = 250;
		pub const MaximumBlockWeight: u32 = 1024;
		pub const MaximumBlockLength: u32 = 2 * 1024;
		pub const AvailableBlockRatio: Perbill = Perbill::one();
	}
	impl frame_system::Trait for TestRuntime {
		type BaseCallFilter = ();
		type Origin = Origin;
		type Index = u64;
		type Call = ();
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = TestEvent;
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type DbWeight = ();
		type BlockExecutionWeight = ();
		type ExtrinsicBaseWeight = ();
		type MaximumExtrinsicWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
		type ModuleToIndex = ();
		type AccountData = ();
		type OnNewAccount = ();
		type OnKilledAccount = ();
		type SystemWeightInfo = ();
	}

	mod generic_event {
		pub use crate::Event;
	}

	impl_outer_event! {
		pub enum TestEvent for TestRuntime {
			generic_event<T>,
			frame_system<T>,
		}
	}

	impl Trait for TestRuntime {
		type Event = TestEvent;
	}

	type TicTacToe = Module<TestRuntime>;
	type System = frame_system::Module<TestRuntime>;

	pub struct ExtBuilder;

	impl ExtBuilder {
		pub fn build() -> TestExternalities {
			let storage = frame_system::GenesisConfig::default()
				.build_storage::<TestRuntime>()
				.unwrap();
			let mut ext = TestExternalities::from(storage);
			ext.execute_with(|| System::set_block_number(1));
			ext
		}
	}

	#[test]
	fn challenger_can_go_first() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// Assert player one can make a move (game 0, top left)
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
		})
	}

	#[test]
	fn challenger_cannot_go_twice() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// Assert player one can make a move (game 0, top left)
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));

			// Assert player one cannot make a second move in a row (game 0, top center)
			assert_noop!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 1), Error::<TestRuntime>::NotYourTurn);
		})
	}

	#[test]
	fn opponent_cannot_steal_square() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// Assert player one can make a move (game 0, top left)
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));

			// Assert player two cannot make the same move (game 0, top left)
			assert_noop!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 0), Error::<TestRuntime>::CellTaken);
		})
	}

	#[test]
	fn vertical_wins_work() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 | 2 |
			// ---------
			// 1 | 2 |
			// ---------
			// 1 |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 1));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 3));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 4));

			// Assert player 1 can claim a win (game 0, column 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(1), 0, 6, Line::Column(0)));
			// The last event should be a win event (game 0, player 1)
			assert_eq!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());

			// Assert players are cleaned up
			let expected : Vec<u64> = vec![];
			assert_eq!(TicTacToe::players(0), expected);

			// Assert board is cleaned up after the win
			assert_eq!(TicTacToe::board(&0, &0), None);
			assert_eq!(TicTacToe::board(&0, &1), None);
			assert_eq!(TicTacToe::board(&0, &2), None);
			assert_eq!(TicTacToe::board(&0, &3), None);
			assert_eq!(TicTacToe::board(&0, &4), None);
			assert_eq!(TicTacToe::board(&0, &5), None);
			assert_eq!(TicTacToe::board(&0, &6), None);
			assert_eq!(TicTacToe::board(&0, &7), None);
			assert_eq!(TicTacToe::board(&0, &8), None);
		})
	}

	#[test]
	fn no_bogus_vertical_wins() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 | 2 |
			// ---------
			// 1 | 2 |
			// ---------
			//   |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 1));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 3));

			// Assert player 1 cannot claim a win she hasn't earned (game 0, column 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(2), 0, 4, Line::Column(0)));
			assert_ne!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());
		})
	}

	#[test]
	fn horizontal_wins_work() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 | 1 | 1
			// ---------
			// 2 | 2 |
			// ---------
			//   |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 3));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 1));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 4));

			// Assert player 1 can claim a win (game 0, row 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(1), 0, 2, Line::Row(0)));
			// The last event should be a win event (gam 0, player 1)
			assert_eq!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());

			// Assert cleanup after win works
			let expected : Vec<u64> = vec![];
			assert_eq!(TicTacToe::players(0), expected);
		})
	}

	#[test]
	fn no_bogus_horizontal_wins() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 | 1 |
			// ---------
			// 2 | 2 |
			// ---------
			//   |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 3));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 1));

			// Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(2), 0, 4, Line::Row(0)));
			assert_ne!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());
		})
	}

	#[test]
	fn uphill_wins_work() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 2 |   | 1
			// ---------
			// 2 | 1 |
			// ---------
			// 1 |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 2));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 4));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 3));

			// Assert player 1 can claim a win (game 0, uphill)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(1), 0, 6, Line::Uphill));
			// The last event should be a win event (game 0, player 1)
			assert_eq!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());

			// Assert cleanup after win works
			let expected : Vec<u64> = vec![];
			assert_eq!(TicTacToe::players(0), expected);
		})
	}

	#[test]
	fn no_bogus_uphill_wins() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 2 |   | 1
			// ---------
			// 2 | 1 |
			// ---------
			//   |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 2));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 4));

			// Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(2), 0, 3, Line::Row(0)));
			assert_ne!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());
		})
	}

	#[test]
	fn downhill_wins_work() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 |   |
			// ---------
			// 2 | 1 |
			// ---------
			// 2 |   | 1

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 3));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 4));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 6));

			// Assert player 1 can claim a win (game 0, uphill)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(1), 0, 8, Line::Downhill));
			// The last event should be a win event (game 0, player 1)
			assert_eq!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());

			// Assert cleanup after win works
			let expected : Vec<u64> = vec![];
			assert_eq!(TicTacToe::players(0), expected);
		})
	}

	#[test]
	fn no_bogus_downhill_wins() {
		ExtBuilder::build().execute_with(|| {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// 1 |   |
			// ---------
			// 2 | 1 |
			// ---------
			// 2 |   |

			// Players make moves according to board diagram
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 0));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(2), 0, 3));
			assert_ok!(TicTacToe::take_normal_turn(Origin::signed(1), 0, 4));

			// Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
			assert_ok!(TicTacToe::take_winning_turn(Origin::signed(2), 0, 6, Line::Row(0)));
			assert_ne!(System::events().last().unwrap().event, RawEvent::Win(0, 1).into());
		})
	}
}
