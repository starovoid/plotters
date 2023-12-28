use fontdue::{Font, FontSettings};

use ttf_parser::Face;

use super::{FontData, FontFamily, FontStyle, LayoutBox};
use std::iter::repeat;

static ARIAL: &'static [u8] = include_bytes!("Arial_regular.ttf");

type FontResult<T> = Result<T, FontError>;

#[derive(Debug, Clone)]
pub enum FontError {
    LockError,
    NoSuchFont(String, String),
    FontLoadError,
    GlyphError,
}

impl std::fmt::Display for FontError {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            FontError::LockError => write!(fmt, "Could not lock mutex"),
            FontError::NoSuchFont(family, style) => {
                write!(fmt, "No such font: {} {}", family, style)
            }
            FontError::FontLoadError => write!(fmt, "Font loading error"),
            FontError::GlyphError => write!(fmt, "Glyph error"),
        }
    }
}

impl std::error::Error for FontError {}

const PLACEHOLDER_CHAR: char = '�';

#[derive(Clone)]
struct FontExt {
    inner: Font,
    face: Option<Face<'static>>,
}

impl Drop for FontExt {
    fn drop(&mut self) {
        // We should make sure the face object dead first
        self.face.take();
    }
}

impl FontExt {
    fn new(font: Font) -> Self {
        Self {
            inner: font,
            face: None,
        }
    }
}

impl std::ops::Deref for FontExt {
    type Target = Font;
    fn deref(&self) -> &Font {
        &self.inner
    }
}

fn load_font_data(_family: FontFamily, _style: FontStyle) -> Result<FontExt, FontError> {
    match Font::from_bytes(ARIAL, FontSettings::default()) {
        Ok(font) => Ok(FontExt::new(font)),
        Err(_) => Err(FontError::FontLoadError),
    }
}

#[derive(Clone)]
pub struct FontDataInternal(FontExt);

impl FontData for FontDataInternal {
    type ErrorType = FontError;

    fn new(family: FontFamily, style: FontStyle) -> Result<Self, FontError> {
        Ok(FontDataInternal(load_font_data(family, style)?))
    }

    fn estimate_layout(&self, size: f64, text: &str) -> Result<LayoutBox, Self::ErrorType> {
        let font = &self.0;
        let pixels_per_em = size as f32 / 1.24;

        let kern = render_single_char('.', font, size as f32)
            .map_err(|_| FontError::GlyphError)?
            .0
            .width
            / 2;

        let mut x_in_unit = (kern * text.len()) as f32;
        for c in text.chars() {
            let metrics = font.metrics(c, size as f32);
            x_in_unit += metrics.advance_width;
        }

        let x_pixels = x_in_unit * pixels_per_em / font.units_per_em();

        Ok(((0, 0), (x_pixels as i32, pixels_per_em as i32)))
    }

    fn draw<E, DrawFunc: FnMut(i32, i32, f32) -> Result<(), E>>(
        &self,
        (base_x, mut base_y): (i32, i32),
        size: f64,
        text: &str,
        mut draw: DrawFunc,
    ) -> Result<Result<(), E>, Self::ErrorType> {
        let em = (size / 1.24) as f32;
        println!("em: {:?}", em);
        let font = &self.0;
        base_y -= (0.24 * em) as i32;

        let (rendered, _bearing) =
            render_text(text, font, em).map_err(|_| FontError::GlyphError)?;
        print_gray_image(&rendered);

        for dy in 0..rendered.height as usize {
            for dx in 0..rendered.width as usize {
                let alpha = rendered.data[dy * rendered.width + dx] as f32 / 255.0;
                if let Err(e) = draw(base_x + dx as i32, base_y + dy as i32, alpha) {
                    return Ok(Err(e));
                }
            }
        }
        Ok(Ok(()))
    }
}

/// Renders text to a 'GrayImage'.
fn render_text(text: &str, font: &FontExt, font_size: f32) -> Result<(GrayImage, i32), ()> {
    let glyphs = render_chars(&font, text, font_size)?;
    join_gray_glyphs(glyphs, &font, font_size)
}

/// Join grayscale glyph images into one image.
fn join_gray_glyphs(
    glyphs: Vec<(GrayImage, i32)>,
    font: &FontExt,
    font_size: f32,
) -> Result<(GrayImage, i32), ()> {
    let mut target_height = 0; // Target height of the final bitmap.
    let mut target_width = 0; // The total width of the glyphs.

    let mut max_bearing = 0i32;
    let mut max_liftup = 0i32;

    for (bm, bearing) in glyphs.iter() {
        target_height = target_height.max(bm.height as usize);
        target_width += bm.width as usize;
        max_bearing = max_bearing.max(*bearing);
        max_liftup = max_liftup.max((bm.height as i32 - bearing).max(0));
    }

    target_height += max_liftup as usize;

    // The space between the characters will be half a dot wide.
    let dot_glyph = render_single_char('.', font, font_size)?;
    let gapsize = (dot_glyph.0.width as f64 / 2.0).ceil() as usize;
    target_width += gapsize * (glyphs.len() - 1);

    let mut encoded_image = Vec::with_capacity(target_width * target_height);
    let mut pixel_streams: Vec<_> = glyphs
        .iter()
        .map(|(img, bearing)| {
            img.data.iter().chain(repeat(&0u8).take(
                (img.width as usize) * ((max_liftup + *bearing - img.height as i32) as usize),
            ))
        })
        .collect();

    for row in 0..target_height {
        for (i, ps) in pixel_streams.iter_mut().enumerate() {
            let height = glyphs[i].0.height as usize;
            let width = glyphs[i].0.width as usize;
            let liftup = (max_liftup + glyphs[i].1 - height as i32).max(0) as usize;

            if row < target_height - height - liftup {
                encoded_image.extend(std::iter::repeat(0u8).take(width));
            } else {
                encoded_image.extend(ps.take(width));
            }

            // Adding a space gap after the character.
            if i + 1 != glyphs.len() {
                encoded_image.extend(std::iter::repeat(0u8).take(gapsize));
            }
        }
    }

    encoded_image.extend(repeat(0u8).take(target_width * target_height - encoded_image.len()));
    assert_eq!(target_height * target_width, encoded_image.len());

    Ok((
        GrayImage {
            width: target_width,
            height: target_height,
            data: encoded_image,
        },
        max_liftup,
    ))
}

/// Get bitmap glyphs of each character of the text.
fn render_chars(font: &FontExt, text: &str, font_size: f32) -> Result<Vec<(GrayImage, i32)>, ()> {
    let mut glyphs = Vec::with_capacity(text.chars().count());
    let space = space_gray_image(font, font_size)?;

    for c in text.chars() {
        if c == ' ' {
            glyphs.push((space.clone(), 1));
        } else {
            glyphs.push(render_single_char(c, font, font_size)?);
        }
    }

    Ok(glyphs)
}

/// Render a `char` to `image::GrayImage`. Also returns y-bearing.
fn render_single_char(c: char, font: &FontExt, font_size: f32) -> Result<(GrayImage, i32), ()> {
    let (metrics, bitmap) = font.rasterize(c, font_size);
    println!(
        "bearing of '{c}': {}, height: {}",
        metrics.height as i32 - metrics.ymin,
        metrics.height
    );

    let img = GrayImage {
        width: metrics.width,
        height: metrics.height,
        data: bitmap,
    };
    Ok((img, metrics.height as i32 - metrics.ymin))
}

/// The space glyph in gray-8 format.
fn space_gray_image(font: &FontExt, font_size: f32) -> Result<GrayImage, ()> {
    let glyph = render_single_char('_', font, font_size)?.0;

    Ok(GrayImage {
        width: glyph.width,
        height: glyph.height,
        data: vec![0; glyph.width],
    })
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct GrayImage {
    pub width: usize,
    pub height: usize,
    pub data: Vec<u8>,
}

/// Print to the terminal for debugging.
pub fn print_gray_image(bm: &GrayImage) {
    println!("y_size: {}, x_size: {}", bm.height, bm.width);
    for i in 0..bm.height as usize {
        for j in 0..bm.width as usize {
            if bm.data[i * bm.width as usize + j] > 60 {
                print!("@");
            } else {
                print!(" ");
            }
        }
        print!("\n")
    }
    println!("");
}
