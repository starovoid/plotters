use super::{FontData, FontFamily, FontStyle, LayoutBox};
use ::core::fmt::{self, Display};
use ::std::error::Error;
use ::std::collections::HashMap;
use ::std::sync::RwLock;
use ::once_cell::sync::Lazy;
use ::ab_glyph::{FontRef, Font, ScaleFont};

static FONTS: Lazy<RwLock<HashMap<String, FontRef<'static>>>> = Lazy::new(|| RwLock::new(HashMap::new()));
pub struct InvalidFont {
    _priv: (),
}

/// Register a font in the fonts table.
pub fn register_font(name: &str, bytes: &'static [u8]) -> Result<(), InvalidFont> {
    let font = FontRef::try_from_slice(bytes).map_err(|_| InvalidFont { _priv: () })?;
    let mut lock = FONTS.write().unwrap();
    lock.insert(name.to_string(), font);
    Ok(())
}

#[derive(Clone)]
pub struct FontDataInternal {
    font_ref: FontRef<'static>
}

#[derive(Debug, Clone)]
pub enum FontError {
    /// No idea what the problem is
    Unknown,
    /// No font data available for the requested family and style.
    FontUnavailable,
}
impl Display for FontError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Since it makes literally no difference to how we'd format
        // this, just delegate to the derived Debug formatter.
        write!(f, "{:?}", self)
    }
}
impl Error for FontError {}

impl FontData for FontDataInternal {
    // TODO: can we rename this to `Error`?
    type ErrorType = FontError;
    fn new(family: FontFamily<'_>, style: FontStyle) -> Result<Self, Self::ErrorType> {
        Ok(Self {
            font_ref: FONTS.read().unwrap().get(family.as_str()).ok_or(FontError::FontUnavailable)?.clone()
        })
    }
    // TODO: ngl, it makes no sense that this uses the same error type as `new`
    fn estimate_layout(&self, size: f64, text: &str) -> Result<LayoutBox, Self::ErrorType> {
        let pixel_per_em = size / 1.24;
        let units_per_em = self.font_ref.units_per_em().unwrap();
        let font = self.font_ref.as_scaled(size as f32);

        let mut x_pixels = 0f32;

        let mut prev = None;
        for c in text.chars() {
            let glyph_id = font.glyph_id(c);
            let size = font.h_advance(glyph_id);
            x_pixels += size;
            if let Some(pc) = prev {
                x_pixels += font.kern(pc, glyph_id);
            }
            prev = Some(glyph_id);
        }

        Ok(((0, 0), (x_pixels as i32, pixel_per_em as i32)))
    }
    fn draw<E, DrawFunc: FnMut(i32, i32, f32) -> Result<(), E>>(
        &self,
        pos: (i32, i32),
        size: f64,
        text: &str,
        mut draw: DrawFunc,
    ) -> Result<Result<(), E>, Self::ErrorType> {
        let font = self.font_ref.as_scaled(size as f32);
        let mut draw = |x: u32, y: u32, c| {
            let (x, y) = (x as i32, y as i32);
            let (base_x, base_y) = pos;
            draw(base_x + x, base_y + y, c)
        };
        let mut x_shift = 0f32;
        let mut prev = None;
        for c in text.chars() {
            if let Some(pc) = prev {
                x_shift += font.kern(font.glyph_id(pc), font.glyph_id(c));
            }
            prev = Some(c);
            let glyph = font.scaled_glyph(c);
            if let Some(q) = font.outline_glyph(glyph) {
                use ::std::panic::{self, AssertUnwindSafe};
                let rect = q.px_bounds();
                // Vertically center the things.
                let y_shift = (size as f32 - rect.height()) / 2.0;
                let y_shift = y_shift as u32;
                let res = panic::catch_unwind(AssertUnwindSafe(|| {
                    q.draw(|x, y, c| {
                        if let Err(_) = draw(x + (x_shift as u32), y + y_shift, c) {
                            panic!("fail")
                        }
                    });
                }));
                if let Err(_) = res {
                    return Err(FontError::Unknown)
                }
                x_shift += font.h_advance(font.glyph_id(c));
            } else {
                x_shift += font.h_advance(font.glyph_id(c));
            }
        }
        Ok(Ok(()))
    }
}
