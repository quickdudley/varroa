use crate::board::Board;
use crate::board::Direction;
use crate::board::Hex;
use sdl2::gfx::primitives::DrawRenderer;
use sdl2::pixels::Color;
use sdl2::render::Canvas;

pub trait BoardTarget {
    fn draw_board(&mut self, board: &Board) -> Result<(), String>;
}

impl<T: sdl2::render::RenderTarget> BoardTarget for Canvas<T> {
    fn draw_board(&mut self, board: &Board) -> Result<(), String> {
        self.set_draw_color(Color::RGB(255, 255, 255));
        self.clear();
        let (w, h) = self.output_size()?;
        let sx = (w / 22) as i16;
        let sy = (h / 34) as i16;
        for (cx, cy) in Hex::all().map(hex2grid) {
            for c in 0..6 {
                let (mut ax, mut ay) = offset(cx, cy, c);
                let (mut bx, mut by) = offset(cx, cy, c + 1);
                ax *= sx;
                ay *= sy;
                bx *= sx;
                by *= sy;
                self.thick_line(ax, ay, bx, by, 4, Color::RGB(0, 0, 0))?;
                self.filled_circle(ax, ay, 2, Color::RGB(0, 0, 0))?;
            }
        }
        for (h, (p, d)) in crate::board::DIRECT.view(board) {
            let (mut cx, mut cy) = hex2grid(h);
            let (hxs, yxs): (Vec<_>, Vec<_>) = (0..6)
                .map(|c| offset(cx, cy, c))
                .map(|(px, py)| (px * sx, py * sy))
                .unzip();
            self.filled_polygon(&hxs, &yxs, Color::RGB(0, 0, 0))?;
            let (mut f1x, mut f1y) = offset(cx, cy, d.into());
            let (mut f2x, mut f2y) =
                offset(cx, cy, <Direction as Into<u8>>::into(d) + 1);
            cx *= sx;
            cy *= sy;
            f1x *= sx;
            f1y *= sy;
            f2x *= sx;
            f2y *= sy;
            self.filled_trigon(cx, cy, f1x, f1y, f2x, f2y, p)?;
        }
        Ok(())
    }
}

fn hex2grid(h: Hex) -> (i16, i16) {
    (
        2 * (h.diag as i16) - (h.row as i16) + 11,
        17 - 3 * (h.row as i16),
    )
}

// sqrt(0.75) * 2
const HYPOTENUSE: f64 = 1.7320508075688772;

pub fn viewport(w: u32, h: u32) -> (u32, u32) {
    let x = w as f64 / 22.0;
    let y = h as f64 / 34.0;
    let o1 = (x, x / HYPOTENUSE);
    let o2 = (y * HYPOTENUSE, y);
    let (sx, sy) = if o1 <= o2 { o1 } else { o2 };
    ((sx * 22.0).floor() as u32, (sy * 34.0).floor() as u32)
}

fn offset(x: i16, y: i16, corner: u8) -> (i16, i16) {
    match corner % 6 {
        0 => (x, y - 2),
        1 => (x + 1, y - 1),
        2 => (x + 1, y + 1),
        3 => (x, y + 2),
        4 => (x - 1, y + 1),
        5 => (x - 1, y - 1),
        _ => unreachable!(),
    }
}
