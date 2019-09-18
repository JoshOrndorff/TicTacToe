/// A runtime module to play censorship-resistant tic-tac-toe games

use support::{decl_module, decl_storage, decl_event, StorageValue, StorageMap, StorageDoubleMap, dispatch::Result};
use system::ensure_signed;
use codec::{ Encode, Decode };
use support::ensure;
use rstd::{vec, vec::{ Vec }};
//use core::convert::TryInto;

/// The module's configuration trait.
pub trait Trait: system::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
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
        NextId get(next_id): GameId;

        // The actual cells of each each game.
		Board get(board): double_map GameId, twox_128(CellIndex) => Option<T::AccountId>;

        // Who is playing in each game
        // This is the canonical way to determine whether a game exists.
        Players get(players): map GameId => Vec<T::AccountId>;

        // Which turn it is in the given game
        Turn get(turn): map GameId => u64;

        // Temporary storage item to keep track of who won each game
        // Ideally the ultimate "result" from a win will be the event
        // or possibly a payout in terms of staked/wagered tokens
        Winner get(winner): map GameId => Option<T::AccountId>
	}
}

// The module's dispatchable functions.
decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Initializing events
		// this is needed only if you are using events in your module
		fn deposit_event() = default;

		// Create a Game
		pub fn create_game(origin, opponent: T::AccountId) -> Result {
			// You can only create games that you will play in
			let challenger = ensure_signed(origin)?;

            // Confirm challenger and opponent are different people
            //ensure!(challenger != opponent, "No playing with yourself");

            // Get the next game id, and update counter in storage
            let game = Self::next_id();
            NextId::put(game.wrapping_add(1));

            // Store the players
            <Players<T>>::insert(game, vec![challenger.clone(), opponent.clone()]);

            // I guess this isn't necessary because None is default. Compiler doesn't like
            // when I try to manually insert None
            // Maybe I need to kill or remove or something instead.
            // for cell in 0..9 {
			// 	<Board<T>>::insert(&game, &cell, &None);
            // }

			// Emit the event
			Self::deposit_event(RawEvent::NewGame(game, challenger, opponent));
			Ok(())
		}

        pub fn take_turn(origin, game: GameId, cell: CellIndex) -> Result {
            let caller = ensure_signed(origin)?;

            // Verify the game id
            ensure!(<Players<T>>::exists(game), "No such Game");

            // Verify the palyer
            let player_index = (Turn::get(game) % 2) as usize;
            let player = <Players<T>>::get(game)[player_index].clone();
            ensure!(caller == player, "Not your turn (or you're not in this game)");

            // Verify the cell
            ensure!(!<Board<T>>::exists(&game, &cell), "Cell already taken");

            // Write to the cell
            <Board<T>>::insert(&game, &cell, &caller);

            // Update the turn counter
            let new_turn = Turn::get(game).wrapping_add(1);
            Turn::insert(game,new_turn);

            // Emit the event
            Self::deposit_event(RawEvent::TurnTaken(game, caller, cell));

            Ok(())
        }

        pub fn claim_win(origin, game: GameId, location: Line) -> Result {
            let caller = ensure_signed(origin)?;

            // Verify the game id
            ensure!(<Players<T>>::exists(game), "No such Game");

            let actually_won = match location {
                Line::Column(n) => Self::check_vertical(game, &caller, n),
                Line::Row(n) => Self::check_horizontal(game, &caller, n),
                Line::Uphill => Self::check_uphill(game, &caller),
                Line::Downhill => Self::check_downhill(game, &caller),
            };

            // If they actually won, emit the event and clean up.
            // Otherwise, just consume their fee.
            if actually_won {
                <Winner<T>>::insert(game, caller.clone());
                Self::deposit_event(RawEvent::Win(game, caller));

                <Board<T>>::remove_prefix(&game);
                <Players<T>>::remove(game);
                Turn::remove(game);
            }

            Ok(())
        }

        //TODO Way to clean up draws and abandoned games
	}
}

impl<T: Trait> Module<T> {
    fn check_vertical(game: GameId, winner: &T::AccountId, col: u8) -> bool {
        for row in 0..3 {
            let cell = row * 3 + col;
            match <Board<T>>::get(&game, &cell) {
                None => { return false; },
                Some(player) => {
                    if &player != winner {
                        return false;
                    }
                },
            }
        }
        true
    }

    fn check_horizontal(game: GameId, winner: &T::AccountId, row: u8) -> bool {
        for col in 0..3 {
            let cell = row * 3 + col;
            match <Board<T>>::get(&game, &cell) {
                None => { return false; },
                Some(player) => {
                    if &player != winner {
                        return false;
                    }
                },
            }
        }
        true
    }

    fn check_uphill(game: GameId, winner: &T::AccountId) -> bool {
        let size = 3;
        for i in 0..size {
            let cell = (size - 1) * (i + 1);
            match <Board<T>>::get(&game, &cell) {
                None => { return false; },
                Some(player) => {
                    if &player != winner {
                        return false;
                    }
                },
            }
        }
        true
    }

    fn check_downhill(game: GameId, winner: &T::AccountId) -> bool {
        let size = 3;
        for i in 0..size {
            let cell = i * (size + 1);
            match <Board<T>>::get(&game, &cell) {
                None => { return false; },
                Some(player) => {
                    if &player != winner {
                        return false;
                    }
                },
            }
        }
        true
    }
}

decl_event!(
	pub enum Event<T> where AccountId = <T as system::Trait>::AccountId {
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

	use runtime_io::with_externalities;
	use primitives::{H256, Blake2Hasher};
	use support::{impl_outer_origin, assert_ok, parameter_types, assert_noop, impl_outer_event};
	use sr_primitives::{traits::{BlakeTwo256, IdentityLookup}, testing::Header};
	use sr_primitives::weights::Weight;
	use sr_primitives::Perbill;

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

    use crate::tictactoe as module;
    impl_outer_event! {
      pub enum TestEvent for Test {
          module<T>,
      }
    }

	// For testing the module, we construct most of a mock runtime. This means
	// first constructing a configuration type (`Test`) which `impl`s each of the
	// configuration traits of modules we want to use.
	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;
	parameter_types! {
		pub const BlockHashCount: u64 = 250;
		pub const MaximumBlockWeight: Weight = 1024;
		pub const MaximumBlockLength: u32 = 2 * 1024;
		pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Call = ();
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type WeightMultiplierUpdate = ();
		type Event = TestEvent;
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
	}
	impl Trait for Test {
		type Event = TestEvent;
	}
	type TicTacToe = Module<Test>;
    type SystemModule = system::Module<Test>;

	// This function basically just builds a genesis storage key/value store according to
	// our desired mockup.
	fn new_test_ext() -> runtime_io::TestExternalities<Blake2Hasher> {
		system::GenesisConfig::default().build_storage::<Test>().unwrap().into()
	}

	#[test]
	fn challenger_can_go_first() {
		with_externalities(&mut new_test_ext(), || {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// Assert player one can make a move (game 0, top left)
			assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
		});
	}

    #[test]
    fn challenger_cannot_go_twice() {
		with_externalities(&mut new_test_ext(), || {
			// Create a new game between players 1 and 2
			assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

			// Assert player one can make a move (game 0, top left)
			assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));

			// Assert player one cannot make a second move in a row (game 0, top center)
			assert_noop!(TicTacToe::take_turn(Origin::signed(1), 0, 1), "Not your turn (or you're not in this game)");
		});
	}

    #[test]
    fn opponent_cannot_steal_square() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // Assert player one can make a move (game 0, top left)
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));

            // Assert player two cannot make the same move (game 0, top left)
            assert_noop!(TicTacToe::take_turn(Origin::signed(2), 0, 0), "Cell already taken");
        });
    }

    #[test]
    fn vertical_wins_work() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 | 2 |
            // ---------
            // 1 | 2 |
            // ---------
            // 1 |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 1));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 6));

            // Assert player 1 can claim a win (game 0, column 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Column(0)));
            // The last event should be a win event (game 0, player 1)
            assert_eq!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());

            // Assert players are cleaned up
            let expected : Vec<u64> = vec![];
            assert_eq!(TicTacToe::players(0), expected);

            // TODO Assert board is cleaned up
            // What do I expect here?
            //assert_eq!(TicTacToe::board(0, 0), 0);
        });
    }

    #[test]
    fn no_bogus_vertical_wins() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 | 2 |
            // ---------
            // 1 | 2 |
            // ---------
            //   |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 1));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 4));

            // Assert player 1 cannot claim a win she hasn't earned (game 0, column 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Column(0)));
            assert_ne!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());
        });
    }

    #[test]
    fn horizontal_wins_work() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 | 1 | 1
            // ---------
            // 2 | 2 |
            // ---------
            //   |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 1));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 2));

            // Assert player 1 can claim a win (game 0, row 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Row(0)));
            // The last event should be a win event (gam 0, player 1)
            assert_eq!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());

            // Assert cleanup after win works
            let expected : Vec<u64> = vec![];
            assert_eq!(TicTacToe::players(0), expected);
        });
    }

    #[test]
    fn no_bogus_horizontal_wins() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 | 1 |
            // ---------
            // 2 | 2 |
            // ---------
            //   |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 1));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 4));

            // Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Row(0)));
            assert_ne!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());
        });
    }

    #[test]
    fn uphill_wins_work() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 2 |   | 1
            // ---------
            // 2 | 1 |
            // ---------
            // 1 |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 2));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 6));

            // Assert player 1 can claim a win (game 0, uphill)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Uphill));
            // The last event should be a win event (game 0, player 1)
            assert_eq!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());

            // Assert cleanup after win works
            let expected : Vec<u64> = vec![];
            assert_eq!(TicTacToe::players(0), expected);
        });
    }

    #[test]
    fn no_bogus_uphill_wins() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 2 |   | 1
            // ---------
            // 2 | 1 |
            // ---------
            //   |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 2));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));

            // Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Row(0)));
            assert_ne!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());
        });
    }

    #[test]
    fn downhill_wins_work() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 |   |
            // ---------
            // 2 | 1 |
            // ---------
            // 2 |   | 1

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 6));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 8));

            // Assert player 1 can claim a win (game 0, uphill)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Downhill));
            // The last event should be a win event (game 0, player 1)
            assert_eq!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());

            // Assert cleanup after win works
            let expected : Vec<u64> = vec![];
            assert_eq!(TicTacToe::players(0), expected);
        });
    }

    #[test]
    fn no_bogus_downhill_wins() {
        with_externalities(&mut new_test_ext(), || {
            // Create a new game between players 1 and 2
            assert_ok!(TicTacToe::create_game(Origin::signed(1), 2));

            // 1 |   |
            // ---------
            // 2 | 1 |
            // ---------
            // 2 |   |

            // Players make moves according to board diagram
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 0));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 3));
            assert_ok!(TicTacToe::take_turn(Origin::signed(1), 0, 4));
            assert_ok!(TicTacToe::take_turn(Origin::signed(2), 0, 6));

            // Assert player 1 cannot claim a win she hasn't earned (game 0, row 0)
            assert_ok!(TicTacToe::claim_win(Origin::signed(1), 0, Line::Row(0)));
            assert_ne!(SystemModule::events().last().unwrap().event, RawEvent::Win(0, 1).into());
        });
    }

}
