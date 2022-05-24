use std::collections::HashMap;

#[cfg_attr(test, derive(Ord, PartialOrd, Hash))]
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Player {
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

impl From<Player> for sdl2::pixels::Color {
    fn from(p: Player) -> Self {
        use Player::*;
        match p {
            Red => sdl2::pixels::Color::RGB(255, 0, 0),
            Green => sdl2::pixels::Color::RGB(0, 255, 0),
            Blue => sdl2::pixels::Color::RGB(0, 0, 255),
        }
    }
}
impl sdl2::gfx::primitives::ToColor for Player {
    fn as_rgba(&self) -> (u8, u8, u8, u8) {
        use Player::*;
        match self {
            Red => (255, 0, 0, 255),
            Green => (0, 255, 0, 255),
            Blue => (0, 0, 255, 255),
        }
    }
}

#[cfg_attr(test, derive(Ord, PartialOrd, Hash))]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Direction {
    NE,
    EE,
    SE,
    SW,
    WW,
    NW,
}

#[cfg_attr(test, derive(Hash))]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Step {
    SL,
    SF,
    SR,
}

#[derive(Copy, Clone, Debug)]
pub enum MoveError {
    WrongPlayer {
        actual: Player,
        current: Player,
        hex: Hex,
    },
    MissingPiece(Hex),
    OffBoard(Hex),
    Opposed(Hex, Hex),
    ExhaustedSkip,
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
            Self::OffBoard(hex) => write!(
                f,
                "Tried to move piece at {} forward but it is facing the edge of the board.",
                hex
            ),
            Self::Opposed(start, fin) => write!(
                f,
                "Tried to move piece at {} forward but it is blocked by the piece at {}.",
                start,
                fin
            ),
            Self::ExhaustedSkip => write!(f, "Tried to skip turn too many times.")
        }
    }
}

impl std::error::Error for MoveError {}

#[cfg_attr(test, derive(Ord, PartialOrd))]
#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct Hex {
    pub row: i8,
    pub diag: i8,
}

impl std::fmt::Display for Hex {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<(), std::fmt::Error> {
        write!(f, "row {}, diagonal {}", self.row, self.diag)
    }
}

#[derive(Clone, Debug)]
pub struct Board {
    pieces: HashMap<Hex, (Player, Direction)>,
    up: Player,
    remainder: u8,
    skipped: u8,
}

impl Board {
    pub fn initial2() -> Self {
        let t = Board::solo();
        Self {
            pieces: t
                .pieces
                .iter()
                .map(|(h, p)| (*h, *p))
                .chain(
                    Orientation {
                        rotation: 3,
                        flip: false,
                    }
                    .view(&t)
                    .map(|(h, (_, d))| (h, (Player::Red, d))),
                )
                .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask() | Player::Red.bitmask(),
            skipped: 0,
        }
    }

    pub fn initial3() -> Self {
        let t = Board::solo();
        Self {
            pieces: (0..)
                .step_by(2)
                .zip(Roster {
                    remainder: 7,
                    start: Player::Blue,
                })
                .flat_map(|(r, p)| {
                    Orientation {
                        rotation: r,
                        flip: false,
                    }
                    .view(&t)
                    .map(move |(h, (_, d))| (h, (p, d)))
                })
                .collect(),
            up: Player::Blue,
            remainder: 7,
            skipped: 0,
        }
    }

    fn solo() -> Self {
        use Direction::*;
        Self {
            pieces: (-5..=-4)
                .flat_map(|row| {
                    (-5..=-3).map(move |diag| (Hex { row, diag }, NE)).chain(
                        (row + 3..=row + 5)
                            .map(move |diag| (Hex { row, diag }, NW)),
                    )
                })
                .map(|(h, d)| (h, (Player::Blue, d)))
                .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        }
    }

    pub fn players(&self) -> impl Iterator<Item = Player> {
        Roster {
            remainder: self.remainder,
            start: self.up,
        }
    }

    pub fn moves(
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
                    3 => Some((SL, SF)),
                    4 => Some((SR, SF)),
                    5 => Some((SF, SL)),
                    6 => Some((SF, SR)),
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
                    .filter_map(move |(s1, s2)| {
                        let (h1, d1) = s1.apply(*h0, *d0);
                        let (h2, d2) = s2.apply(h1, d1);
                        let mut tfp = true;
                        if h1.on_board()
                            && match self.pieces.get(&h1) {
                                None => true,
                                Some((_, dt)) => dt.flip() != d1,
                            }
                            && h2.on_board()
                            && match self.pieces.get(&h2) {
                                None => true,
                                Some((pt, dt)) => {
                                    tfp = *pt != self.up
                                        || [SL, SR].into_iter().all(|st| {
                                            st.apply(h2, *dt).1 != d1
                                        });
                                    dt.flip() != d2
                                }
                            }
                            && (tfp || s1 == SF || s2 != SF || {
                                let h3 = h0.neighbour(*d0);
                                !h3.on_board()
                                    || match self.pieces.get(&h3) {
                                        None => true,
                                        Some((pt, dt)) => {
                                            *pt != self.up || dt != d0
                                        }
                                    }
                            })
                        {
                            Some(Some(((*h0, s1), (h1, s2))))
                        } else {
                            None
                        }
                    })
                    .chain(
                        r.map(move |(h1, (_, d1))| ((h0, d0), (h1, d1)))
                            .flat_map(|((h0, d0), (h1, d1))| {
                                Step::all().flat_map(move |s0| {
                                    Step::all().map(move |s1| {
                                        ((h0, d0, s0), (h1, d1, s1))
                                    })
                                })
                            })
                            .filter_map(|(t0, t1)| {
                                let mut i =
                                    [t0, t1].into_iter().map(|(h, d, s)| {
                                        let (hd, dd) = s.apply(*h, *d);
                                        (h, d, s, hd, dd)
                                    });
                                let q0 = i.next()?;
                                let q1 = i.next()?;
                                if [q0, q1].into_iter().all(|q| q.3.on_board())
                                {
                                    Some((q0, q1))
                                } else {
                                    None
                                }
                            })
                            .flat_map(|(t0, t1)| {
                                [(t0, t1, false), (t1, t0, true)]
                            })
                            .filter_map(|(t0, t1, careful)| {
                                if !(t0.3 == *t1.0
                                    || careful && t0.3 != t1.3 && *t0.0 != t1.3
                                    || t0.2 == SR
                                        && t1.3 == *t0.0
                                        && t0.1.turn(5).flip() != t1.4
                                    || t0.2 != SF
                                        && match self.pieces.get(&t0.3) {
                                            None => false,
                                            Some((_, dt)) => dt.flip() == t0.4,
                                        }
                                    || t1.2 != SF
                                        && if t0.3 == t1.3 {
                                            t0.4.flip() == *t1.1
                                        } else if let Some((_, dt)) =
                                            self.pieces.get(&t1.3)
                                        {
                                            dt.flip() == *t1.1
                                        } else {
                                            false
                                        })
                                {
                                    Some(Some(((*t0.0, t0.2), (*t1.0, t1.2))))
                                } else {
                                    None
                                }
                            }),
                    )
            })
            .chain(std::iter::once(None))
    }

    pub fn step(
        &mut self,
        m1: (Hex, Step),
        m2: (Hex, Step),
    ) -> Result<(), MoveError> {
        enum Rollback {
            Delete(Hex),
            Insert(Hex, Player, Direction),
        }
        if m1.1 != Step::SF && m2.1 != Step::SF && m1.1 != m2.1 && m1.0 == m2.0
        {
            return self.skip();
        }
        self.skipped = 0;
        let mut changes = Vec::new();
        for r in [m1, m2].into_iter().map(|(h, s)| {
            let (p, d) =
                self.pieces.get(&h).ok_or(MoveError::MissingPiece(h))?;
            let (p, d) = (*p, *d);
            if p != self.up {
                Err(MoveError::WrongPlayer {
                    actual: p,
                    current: self.up,
                    hex: h,
                })
            } else {
                Ok(())
            }?;
            let (h1, d1) = s.apply(h, d);
            if h1.on_board() {
                Ok(())
            } else {
                Err(MoveError::OffBoard(h))
            }?;
            if h == h1 {
                changes.push(Rollback::Insert(h, p, d));
                self.pieces.insert(h, (p, d1));
            } else {
                match self.pieces.get(&h1) {
                    None => {
                        changes.push(Rollback::Delete(h1));
                        Ok(())
                    }
                    Some((pt, dt)) => {
                        if dt.flip() == d1 {
                            Err(MoveError::Opposed(h, h1))
                        } else {
                            changes.push(Rollback::Insert(h1, *pt, *dt));
                            Ok(())
                        }
                    }
                }?;
                changes.push(Rollback::Insert(h, p, d));
                self.pieces.insert(h1, (p, d1));
                self.pieces.remove(&h);
            }
            Ok(())
        }) {
            match r {
                Ok(_) => (),
                Err(e) => {
                    for c in changes.into_iter().rev() {
                        match c {
                            Rollback::Delete(h) => {
                                self.pieces.remove(&h);
                            }
                            Rollback::Insert(h, p, d) => {
                                self.pieces.insert(h, (p, d));
                            }
                        }
                    }
                    return Err(e);
                }
            }
        }
        let mut to_check = changes.into_iter().fold(0, |t, c| {
            t | match c {
                Rollback::Insert(_, p, _) => {
                    if p == self.up {
                        0
                    } else {
                        p.bitmask()
                    }
                }
                _ => 0,
            }
        });
        if to_check != 0 {
            for (p, _) in self.pieces.values() {
                to_check &= !p.bitmask();
                if to_check == 0 {
                    break;
                }
            }
        }
        self.remainder &= !to_check;
        if let Some(up) = self.players().nth(1) {
            self.up = up;
        }
        Ok(())
    }

    pub fn skip(&mut self) -> Result<(), MoveError> {
        let up = self.up;
        if up.bitmask() & self.skipped != 0 {
            return Err(MoveError::ExhaustedSkip);
        }
        if let Some(next) = self.players().nth(1) {
            self.skipped = self.skipped | up.bitmask();
            self.up = next;
        }
        Ok(())
    }
}

pub const DIRECT: Orientation = Orientation {
    rotation: 0,
    flip: false,
};

pub struct Orientation {
    rotation: u8,
    flip: bool,
}

impl Orientation {
    pub fn all() -> impl Iterator<Item = Self> {
        (0..6).flat_map(|rotation| {
            std::iter::once(Self {
                rotation,
                flip: false,
            })
            .chain(std::iter::once(Self {
                rotation,
                flip: true,
            }))
        })
    }

    pub fn view<'s, 'a>(
        &'s self,
        board: &'a Board,
    ) -> impl Iterator<Item = (Hex, (Player, Direction))> + 'a {
        let r = self.rotation;
        let f = self.flip;
        board.pieces.iter().map(move |(h, (p, d))| {
            let rh = h.rotate(r);
            let rd = d.turn(6 - r % 6);
            if f {
                (rh.reflect(), (*p, rd.reflect()))
            } else {
                (rh, (*p, rd))
            }
        })
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

    fn reflect(self) -> Self {
        Self {
            row: self.row,
            diag: self.row - self.diag,
        }
    }

    fn rotate(self, n: u8) -> Self {
        let (a, b, c, d) = match n % 6 {
            0 => (1, 0, 0, 1),
            1 => (1, 1, -1, 0),
            2 => (0, 1, -1, -1),
            3 => (-1, 0, 0, -1),
            4 => (-1, -1, 1, 0),
            5 => (0, -1, 1, 1),
            _ => unreachable!(),
        };
        Self {
            row: b * self.diag + d * self.row,
            diag: a * self.diag + c * self.row,
        }
    }

    pub fn all() -> impl Iterator<Item = Self> {
        (-5..=0)
            .flat_map(|row| (-5..=row + 5).map(move |diag| Self { row, diag }))
            .chain((1..=5).flat_map(|row| {
                (row - 5..=5).map(move |diag| Self { row, diag })
            }))
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
            Self::Red => Self::Blue,
            Self::Green => Self::Red,
            Self::Blue => Self::Green,
        }
    }
}

impl Direction {
    fn turn(self, r: u8) -> Self {
        (<Self as Into<u8>>::into(self) + r).into()
    }

    fn flip(self) -> Self {
        self.turn(3)
    }

    fn reflect(self) -> Self {
        (5 - <Self as Into<u8>>::into(self)).into()
    }
}

impl From<Direction> for u8 {
    fn from(source: Direction) -> u8 {
        use Direction::*;
        match source {
            NE => 0,
            EE => 1,
            SE => 2,
            SW => 3,
            WW => 4,
            NW => 5,
        }
    }
}

impl From<u8> for Direction {
    fn from(source: u8) -> Self {
        use Direction::*;
        match source % 6 {
            0 => NE,
            1 => EE,
            2 => SE,
            3 => SW,
            4 => WW,
            5 => NW,
            _ => unreachable!(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use Direction::*;

    #[test]
    fn test_moves_1() {
        test_moves(Board {
            pieces: [(Hex { row: 0, diag: 0 }, (Player::Blue, NE))]
                .into_iter()
                .collect(),
            remainder: Player::Blue.bitmask(),
            up: Player::Blue,
            skipped: 0,
        });
    }

    #[test]
    fn test_moves_edge() {
        test_moves(Board {
            pieces: [(Hex { row: -4, diag: -4 }, (Player::Blue, SW))]
                .into_iter()
                .collect(),
            remainder: Player::Blue.bitmask(),
            up: Player::Blue,
            skipped: 0,
        });
    }

    #[test]
    fn test_moves_independent() {
        test_moves(Board {
            pieces: [
                (Hex { row: -2, diag: -2 }, (Player::Blue, SW)),
                (Hex { row: 2, diag: 2 }, (Player::Blue, NE)),
            ]
            .into_iter()
            .collect(),
            remainder: Player::Blue.bitmask(),
            up: Player::Blue,
            skipped: 0,
        });
    }

    #[test]
    fn test_moves_opposed() {
        test_moves(Board {
            pieces: [
                (Hex { row: 0, diag: 0 }, (Player::Blue, NE)),
                (Hex { row: 1, diag: 1 }, (Player::Blue, SW)),
            ]
            .into_iter()
            .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        });
    }

    #[test]
    fn test_moves_converge() {
        test_moves(Board {
            pieces: [
                (Hex { row: 0, diag: 0 }, (Player::Blue, NE)),
                (Hex { row: 0, diag: 1 }, (Player::Blue, NW)),
            ]
            .into_iter()
            .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        });
    }

    #[test]
    fn test_moves_initial() {
        test_moves(Board::initial2());
        test_moves(Board::initial3());
    }

    #[test]
    fn test_moves_middle() {
        test_moves(Board {
            pieces: [
                (Hex { row: -5, diag: -3 }, (Player::Blue, NE)),
                (Hex { row: -4, diag: -3 }, (Player::Blue, NE)),
            ]
            .into_iter()
            .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        })
    }

    #[test]
    fn test_moves_line() {
        test_moves(Board {
            pieces: [
                (Hex { row: -5, diag: -5 }, (Player::Blue, NE)),
                (Hex { row: -4, diag: -4 }, (Player::Blue, NE)),
            ]
            .into_iter()
            .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        })
    }

    fn test_moves(board: Board) {
        use std::collections::BTreeMap;
        use std::collections::HashSet;
        let mut lookback = HashMap::new();
        let mut unreached: HashSet<BTreeMap<Hex, (Player, Direction)>> =
            Hex::all()
                .flat_map(|h0| Step::all().map(move |s0| (h0, s0)))
                .flat_map(|m0| {
                    Hex::all()
                        .flat_map(|h0| Step::all().map(move |s0| (h0, s0)))
                        .map(move |m1| (m0, m1))
                })
                .filter_map(|(m0, m1)| {
                    let mut t = board.clone();
                    t.step(m0, m1).ok()?;
                    let result: BTreeMap<_, _> = DIRECT.view(&t).collect();
                    lookback.insert(result.clone(), (m0, m1));
                    Some(result)
                })
                .collect();
        unreached.remove(&DIRECT.view(&board).collect());
        for (m0, m1) in board.moves().flatten() {
            let mut t = board.clone();
            match t.step(m0, m1) {
                Ok(_) => {
                    let h = DIRECT.view(&t).collect();
                    if unreached.contains(&h) {
                        unreached.remove(&h);
                        lookback.insert(h, (m0, m1));
                    } else {
                        panic!(
                            "Board = {:?}\nDuplicate move: {:?}, {:?} vs {:?}",
                            &board,
                            m0,
                            m1,
                            lookback.get(&h)
                        );
                    }
                }
                Err(e) => {
                    panic!("Board = {:?}\n{}: {:?}, {:?}.", &board, e, m0, m1);
                }
            }
        }
        for u in unreached {
            panic!("Board = {:?}\nMissing move: {:?}", &board, lookback.get(&u))
        }
    }

    #[test]
    fn test_step_sl() {
        use Step::*;
        let mut t = Board {
            pieces: [(Hex { row: 0, diag: 0 }, (Player::Blue, NE))]
                .into_iter()
                .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        };
        assert!(t
            .step((Hex { row: 0, diag: 0 }, SL), (Hex { row: 0, diag: 0 }, SL))
            .is_ok());
        assert_eq!(
            t.pieces.get(&Hex { row: 0, diag: 0 }),
            Some(&(Player::Blue, WW))
        );
    }

    #[test]
    fn test_step_fl() {
        use Step::*;
        let mut t = Board {
            pieces: [(Hex { row: 0, diag: 0 }, (Player::Blue, NE))]
                .into_iter()
                .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        };
        assert!(t
            .step((Hex { row: 0, diag: 0 }, SF), (Hex { row: 1, diag: 1 }, SL))
            .is_ok());
        assert_eq!(t.pieces.get(&Hex { row: 0, diag: 0 }), None);
        assert_eq!(
            t.pieces.get(&Hex { row: 1, diag: 1 }),
            Some(&(Player::Blue, NW))
        );
    }

    #[test]
    fn test_step_lf() {
        use Step::*;
        let mut t = Board {
            pieces: [(Hex { row: 0, diag: 0 }, (Player::Blue, NE))]
                .into_iter()
                .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        };
        assert!(t
            .step((Hex { row: 0, diag: 0 }, SL), (Hex { row: 0, diag: 0 }, SF))
            .is_ok());
        assert_eq!(t.pieces.get(&Hex { row: 0, diag: 0 }), None);
        assert_eq!(
            t.pieces.get(&Hex { row: 1, diag: 0 }),
            Some(&(Player::Blue, NW))
        );
    }

    #[test]
    fn test_tails() {
        let mut i = [1, 2, 3, 4].into_iter().tails();
        match i.next() {
            Some((x, r)) => {
                assert_eq!(x, 1);
                assert_eq!(r.collect::<Vec<_>>(), vec![2, 3, 4]);
            }
            None => {
                assert!(false);
            }
        }
        match i.next() {
            Some((x, r)) => {
                assert_eq!(x, 2);
                assert_eq!(r.collect::<Vec<_>>(), vec![3, 4]);
            }
            None => {
                assert!(false);
            }
        }
    }

    #[test]
    fn test_off_board_blocked() {
        let h1 = Hex { row: -4, diag: -4 };
        let h2 = Step::SF.apply(h1, Direction::SW).0;
        let mut board = Board {
            pieces: [(h1, (Player::Blue, Direction::SW))].into_iter().collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        };
        match board.step((h1, Step::SF), (h2, Step::SF)) {
            Ok(_) => {
                panic!("Moving piece off board succeeded");
            }
            Err(MoveError::OffBoard(_)) => {}
            _ => {
                panic!("Failed for wrong reason");
            }
        }
    }

    #[test]
    fn test_front_blocked() {
        let board = Board {
            pieces: [
                (Hex { row: 0, diag: 0 }, (Player::Blue, Direction::EE)),
                (Hex { row: 0, diag: 1 }, (Player::Blue, Direction::WW)),
            ]
            .into_iter()
            .collect(),
            up: Player::Blue,
            remainder: Player::Blue.bitmask(),
            skipped: 0,
        };
        assert_eq!(
            board
                .moves()
                .flatten()
                .filter(|((_, s0), _)| *s0 == Step::SF)
                .count(),
            0
        );
    }
}
