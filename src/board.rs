use std::collections::HashMap;

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
enum Player {
    Red,
    Green,
    Blue,
}

impl std::fmt::Display for Player {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        <Self as std::fmt::Debug>::fmt(self, f)
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum Direction {
    NE,
    EE,
    SE,
    SW,
    WW,
    NW,
}

#[derive(Copy, Clone, PartialEq)]
enum Step {
    SL,
    SF,
    SR,
}

#[derive(Copy, Clone, Debug)]
enum MoveError {
    WrongPlayer {
        actual: Player,
        current: Player,
        hex: Hex,
    },
    MissingPiece(Hex),
}

impl std::fmt::Display for MoveError {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        match self {
            Self::WrongPlayer {
                actual,
                current,
                hex,
            } => write!(
                f,
                "Tried to move piece at {} which belongs to {}; but the current player is {}.",
                hex, actual, current
            ),
            Self::MissingPiece(hex) => write!(
                f,
                "Tried to move piece at {} but there is no piece there.",
                hex
            ),
        }
    }
}

impl std::error::Error for MoveError {}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
struct Hex {
    row: i8,
    diag: i8,
}

impl std::fmt::Display for Hex {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "row {}, diagonal {}", self.row, self.diag)
    }
}

struct Board {
    pieces: HashMap<Hex, (Player, Direction)>,
    up: Player,
    remainder: u8,
}

impl Board {
    fn players(&self) -> Roster {
        Roster {
            remainder: self.remainder,
            start: self.up,
        }
    }

    fn moves(
        &self,
    ) -> impl Iterator<Item = Option<((Hex, Step), (Hex, Step))>> + '_ {
        use Step::*;
        struct OnePiece(u8);
        impl Iterator for OnePiece {
            type Item = (Step, Step);
            fn next(&mut self) -> Option<(Step, Step)> {
                let value = match self.0 {
                    0 => Some((SL, SL)),
                    1 => Some((SF, SF)),
                    2 => Some((SR, SR)),
                    4 => Some((SL, SF)),
                    5 => Some((SR, SF)),
                    6 => Some((SF, SL)),
                    7 => Some((SF, SR)),
                    _ => None,
                }?;
                self.0 += 1;
                Some(value)
            }
        }
        self.pieces
            .iter()
            .filter(|(_, (p1, _))| *p1 == self.up)
            .tails()
            .flat_map(move |((h0, (_, d0)), r)| {
                OnePiece(0)
                    .filter_map(|(s1, s2)| {
                        let mut hn = *h0;
                        let mut dn = *d0;
                        if [s1, s2].into_iter().all(|s| {
                            (hn, dn) = s.apply(hn, dn);
                            hn.on_board()
                                && match self.pieces.get(&hn) {
                                    None => true,
                                    Some((_, dt)) => dt.flip() != dn,
                                }
                        }) {
                            Some(Some(((*h0, s1), (*h0, s2))))
                        } else {
                            None
                        }
                    })
                    .chain(
                        Step::all()
                            .filter_map(|s0| {
                                let (ha, da) = s0.apply(*h0, *d0);
                                if ha.on_board()
                                    && match self.pieces.get(&ha) {
                                        None => true,
                                        Some((_, dt)) => dt.flip() != *d0,
                                    }
                                {
                                    Some((s0, ha, da))
                                } else {
                                    None
                                }
                            })
                            .flat_map(move |(s0, ha, da)| {
                                r.clone().flat_map(move |(h1, (_, d1))| {
                                    Step::all()
                                        .filter_map(move |s1| {
                                            let (hb, db) = s1.apply(*h1, *d1);
                                            if hb.on_board()
                                                && match self.pieces.get(&ha) {
                                                    None => true,
                                                    Some((_, dt)) => {
                                                        dt.flip() != *d1
                                                    }
                                                }
                                            {
                                                Some((s1, hb, db))
                                            } else {
                                                None
                                            }
                                        })
                                        .flat_map(move |(s1, hb, db)| {
                                            std::iter::once(Some((
                                                (*h0, s0),
                                                (*h1, s1),
                                            )))
                                            .chain(
                                                if *h0 == *h1
                                                    || *h0 == hb
                                                    || *h1 == ha
                                                    || ha == hb
                                                {
                                                    Some(Some((
                                                        (*h1, s1),
                                                        (*h0, s0),
                                                    )))
                                                } else {
                                                    None
                                                },
                                            )
                                        })
                                })
                            }),
                    )
            })
            .chain(std::iter::once(None))
    }

    fn step(
        &mut self,
        m1: (Hex, Step),
        m2: (Hex, Step),
    ) -> Result<(), MoveError> {
        todo!()
    }
}

/* (row, diagonal)
 *   (1,0)  (1,1)
 * (0,-1) (0,0) (0,1)
 *   (-1,-1)  (-1,0)
 */

impl Hex {
    fn neighbour(self, d: Direction) -> Self {
        use Direction::*;
        match d {
            NE => Self {
                row: self.row + 1,
                diag: self.diag + 1,
            },
            EE => Self {
                row: self.row,
                diag: self.diag + 1,
            },
            SE => Self {
                row: self.row - 1,
                diag: self.diag,
            },
            SW => Self {
                row: self.row - 1,
                diag: self.diag - 1,
            },
            WW => Self {
                row: self.row,
                diag: self.diag - 1,
            },
            NW => Self {
                row: self.row + 1,
                diag: self.diag,
            },
        }
    }

    fn on_board(self) -> bool {
        self.row >= -5
            && self.row <= 5
            && self.diag >= -5
            && self.diag <= 5
            && self.diag >= self.row - 5
            && self.diag <= self.row + 5
    }
}

impl Player {
    fn bitmask(self) -> u8 {
        match self {
            Self::Red => 1,
            Self::Green => 2,
            Self::Blue => 4,
        }
    }

    fn rotate(self) -> Self {
        match self {
            Self::Red => Self::Green,
            Self::Green => Self::Blue,
            Self::Blue => Self::Red,
        }
    }
}

impl Direction {
    fn turn(self, r: u8) -> Self {
        use Direction::*;
        let d = match self {
            NE => 0,
            EE => 1,
            SE => 2,
            SW => 3,
            WW => 4,
            NW => 5,
        };
        match (d + r) % 6 {
            0 => NE,
            1 => EE,
            2 => SE,
            3 => SW,
            4 => WW,
            5 => NW,
            _ => unreachable!(),
        }
    }

    fn flip(self) -> Self {
        self.turn(3)
    }
}

struct Roster {
    remainder: u8,
    start: Player,
}

impl Iterator for Roster {
    type Item = Player;
    fn next(&mut self) -> Option<Player> {
        if self.remainder == 0 {
            None
        } else {
            loop {
                let who = self.start;
                let mask = who.bitmask();
                self.start = who.rotate();
                if mask & self.remainder != 0 {
                    self.remainder &= !mask;
                    break Some(who);
                }
            }
        }
    }
}

impl Step {
    fn apply(self, p: Hex, d: Direction) -> (Hex, Direction) {
        use Step::*;
        match self {
            SL => (p, d.turn(5)),
            SF => (p.neighbour(d), d),
            SR => (p, d.turn(1)),
        }
    }

    fn all() -> impl Iterator<Item = Step> {
        struct I {
            i: u8,
        }
        impl Iterator for I {
            type Item = Step;
            fn next(&mut self) -> Option<Step> {
                use Step::*;
                match self.i {
                    0 => Some(SL),
                    1 => Some(SF),
                    2 => Some(SR),
                    _ => None,
                }
                .map(|a| {
                    self.i += 1;
                    a
                })
            }
        }
        I { i: 0 }
    }
}

trait CloneIteratorExtra: Clone + Iterator {
    fn tails(self) -> Tails<Self> {
        Tails(self)
    }
}

impl<I: Clone + Iterator> CloneIteratorExtra for I {}

struct Tails<I>(I);

impl<I: Clone + Iterator> Iterator for Tails<I> {
    type Item = (<I as Iterator>::Item, I);
    fn next(&mut self) -> Option<Self::Item> {
        let value = self.0.next()?;
        Some((value, self.0.clone()))
    }
}