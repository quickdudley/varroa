use sdl2::event::Event;
use sdl2::keyboard::Keycode;

mod board;
mod drawing;

use board::Board;
use drawing::BoardTarget;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;
    let (w, h) = drawing::viewport(1024, 768);
    println!("w: {}, h: {}", w, h);
    let window = video_subsystem
        .window("Varroa", w, h)
        .position_centered()
        .build()?;
    let mut canvas = window.into_canvas().build()?;
    let board = Board::initial2();
    let mut event_pump = sdl_context.event_pump()?;
    'running: loop {
        canvas.draw_board(&board)?;
        canvas.present();
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }
    }
    Ok(())
}
