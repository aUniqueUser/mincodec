#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "alloc")]
extern crate alloc;

use bitbuf::{BitBuf, BitBufMut, CopyError, Insufficient};
use core::{
    marker::{PhantomData, PhantomPinned},
    mem::size_of,
};
use void::Void;

pub trait MinCodec: Sized {
    type WriteError;
    type ReadError;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError>;

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError>;
}

macro_rules! impl_primitives {
    () => {
        impl_primitives! {
            as numbers {
                u8 u16 u32 u64 u128
                i8 i16 i32 i64 i128
                f32 f64
            }
        }
    };
    ($(as $cat:ident { $($ty:ty)* })*) => {
        $(impl_primitives!($cat $($ty)*);)*
    };
    (numbers $($ty:ty)*) => {
        $(
            impl MinCodec for $ty {
                type ReadError = Insufficient;
                type WriteError = Insufficient;

                fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
                    buf.put_aligned(self.to_be_bytes().as_ref())
                }

                fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
                    let mut bytes = [0u8; size_of::<Self>()];
                    buf.copy_aligned(&mut bytes)?;
                    Ok(Self::from_be_bytes(bytes))
                }
            }
        )*
    };
}

impl MinCodec for usize {
    type ReadError = Insufficient;
    type WriteError = Insufficient;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        buf.put_aligned((self as u64).to_be_bytes().as_ref())
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        let mut bytes = [0u8; size_of::<u64>()];
        buf.copy_aligned(&mut bytes)?;
        Ok(u64::from_be_bytes(bytes) as usize)
    }
}

impl MinCodec for isize {
    type ReadError = Insufficient;
    type WriteError = Insufficient;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        buf.put_aligned((self as i64).to_be_bytes().as_ref())
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        let mut bytes = [0u8; size_of::<i64>()];
        buf.copy_aligned(&mut bytes)?;
        Ok(i64::from_be_bytes(bytes) as isize)
    }
}

impl MinCodec for char {
    type ReadError = Insufficient;
    type WriteError = Insufficient;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        buf.put_aligned((self as u8).to_be_bytes().as_ref())
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        let mut bytes = [0u8; size_of::<u8>()];
        buf.copy_aligned(&mut bytes)?;
        Ok(u8::from_be_bytes(bytes) as char)
    }
}

impl_primitives!();

impl MinCodec for bool {
    type WriteError = Insufficient;
    type ReadError = Insufficient;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        buf.push(self)
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        buf.pop().ok_or(Insufficient)
    }
}

impl MinCodec for () {
    type WriteError = Void;
    type ReadError = Void;

    fn write<B: BitBufMut>(self, _: &mut B) -> Result<(), Self::WriteError> {
        Ok(())
    }

    fn read<B: BitBuf>(_: &mut B) -> Result<Self, Self::ReadError> {
        Ok(())
    }
}

impl<T> MinCodec for [T; 0] {
    type WriteError = Void;
    type ReadError = Void;

    fn write<B: BitBufMut>(self, _: &mut B) -> Result<(), Self::WriteError> {
        Ok(())
    }

    fn read<B: BitBuf>(_: &mut B) -> Result<Self, Self::ReadError> {
        Ok([])
    }
}

impl<T: ?Sized> MinCodec for PhantomData<T> {
    type WriteError = Void;
    type ReadError = Void;

    fn write<B: BitBufMut>(self, _: &mut B) -> Result<(), Self::WriteError> {
        Ok(())
    }

    fn read<B: BitBuf>(_: &mut B) -> Result<Self, Self::ReadError> {
        Ok(PhantomData)
    }
}

impl MinCodec for PhantomPinned {
    type WriteError = Void;
    type ReadError = Void;

    fn write<B: BitBufMut>(self, _: &mut B) -> Result<(), Self::WriteError> {
        Ok(())
    }

    fn read<B: BitBuf>(_: &mut B) -> Result<Self, Self::ReadError> {
        Ok(PhantomPinned)
    }
}

macro_rules! array_impls {
    ($($len:tt)+) => {
        $(
            impl<T> MinCodec for [T; $len]
            where
                T: MinCodec,
            {
                type WriteError = T::WriteError;
                type ReadError = T::ReadError;

                fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
                    use core::ptr::read;

                    for i in 0..$len {
                        unsafe { read(&self[i] as *const T) }.write(buf)?;
                    }

                    Ok(())
                }

                fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
                    use core::{mem::MaybeUninit, ptr::write};

                    let mut data: MaybeUninit<[T; $len]> = MaybeUninit::uninit();
                    let ptr = data.as_mut_ptr() as *mut T;

                    for _ in 0..$len {
                        unsafe {
                            write(ptr, T::read(buf)?);
                            ptr.add(1);
                        }
                    }

                    Ok(unsafe { data.assume_init() })
                }
            }
        )+
    }
}

array_impls! {
    01 02 03 04 05 06 07 08 09 10
    11 12 13 14 15 16 17 18 19 20
    21 22 23 24 25 26 27 28 29 30
    31 32
}

impl<T: MinCodec> MinCodec for (T,) {
    type WriteError = T::WriteError;
    type ReadError = T::ReadError;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        self.0.write(buf)?;
        Ok(())
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        Ok((T::read(buf)?,))
    }
}

macro_rules! tuple_impls {
    ($($we:ident $re:ident $len:expr => ($($n:tt $name:ident)+))+) => {
        $(
            #[derive(Debug)]
            #[doc(hidden)]
            pub enum $we<$($name),+>
            where
                $($name: MinCodec,)+
            {
                $($name($name::WriteError),)*
            }

            #[derive(Debug)]
            #[doc(hidden)]
            pub enum $re<$($name),+>
            where
                $($name: MinCodec,)+
            {
                $($name($name::ReadError),)*
            }

            impl<$($name),+> MinCodec for ($($name,)+)
            where
                $($name: MinCodec,)+
            {
                type WriteError = $we<$($name),+>;
                type ReadError = $re<$($name),+>;

                fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
                    $(
                        self.$n.write(buf).map_err($we::$name)?;
                    )+
                    Ok(())
                }

                fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
                    Ok(($(
                        $name::read(buf).map_err($re::$name)?
                    ),+))
                }
            }
        )+
    }
}

tuple_impls! {
    Enum1WriteError  Enum1ReadError  2 => (0 T0 1 T1)
    Enum2WriteError  Enum2ReadError  3 => (0 T0 1 T1 2 T2)
    Enum3WriteError  Enum3ReadError  4 => (0 T0 1 T1 2 T2 3 T3)
    Enum4WriteError  Enum4ReadError  5 => (0 T0 1 T1 2 T2 3 T3 4 T4)
    Enum5WriteError  Enum5ReadError  6 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5)
    Enum6WriteError  Enum6ReadError  7 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6)
    Enum7WriteError  Enum7ReadError  8 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7)
    Enum8WriteError  Enum8ReadError  9 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8)
    Enum9WriteError  Enum9ReadError  10 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9)
    Enum10WriteError Enum10ReadError 11 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10)
    Enum11WriteError Enum11ReadError 12 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11)
    Enum12WriteError Enum12ReadError 13 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12)
    Enum13WriteError Enum13ReadError 14 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13)
    Enum14WriteError Enum14ReadError 15 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14)
    Enum15WriteError Enum15ReadError 16 => (0 T0 1 T1 2 T2 3 T3 4 T4 5 T5 6 T6 7 T7 8 T8 9 T9 10 T10 11 T11 12 T12 13 T13 14 T14 15 T15)
}

#[derive(Debug)]
pub enum OptionWriteError<T> {
    Buf(Insufficient),
    Write(T),
}

impl<T> From<Insufficient> for OptionWriteError<T> {
    fn from(error: Insufficient) -> Self {
        OptionWriteError::Buf(error)
    }
}

#[derive(Debug)]
pub enum OptionReadError<T> {
    Buf(CopyError),
    Read(T),
}

impl<T> From<T> for OptionReadError<T> {
    fn from(error: T) -> Self {
        OptionReadError::Read(error)
    }
}

impl<T: MinCodec> MinCodec for Option<T> {
    type ReadError = OptionReadError<T::ReadError>;
    type WriteError = OptionWriteError<T::WriteError>;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        Ok(match self {
            None => buf.push(false)?,
            Some(item) => {
                buf.push(true)?;
                item.write(buf).map_err(OptionWriteError::Write)?;
            }
        })
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        Ok(
            if buf
                .pop()
                .ok_or(CopyError::Insufficient(Insufficient))
                .map_err(OptionReadError::Buf)?
            {
                Some(T::read(buf)?)
            } else {
                None
            },
        )
    }
}

#[derive(Debug)]
pub enum ResultWriteError<T, E> {
    Buf(Insufficient),
    WriteOk(T),
    WriteErr(E),
}

impl<T, E> From<Insufficient> for ResultWriteError<T, E> {
    fn from(error: Insufficient) -> Self {
        ResultWriteError::Buf(error)
    }
}

#[derive(Debug)]
pub enum ResultReadError<T, E> {
    Buf(CopyError),
    ReadOk(T),
    ReadErr(E),
}

impl<T, E> From<CopyError> for ResultReadError<T, E> {
    fn from(error: CopyError) -> Self {
        ResultReadError::Buf(error)
    }
}

impl<T: MinCodec, E: MinCodec> MinCodec for Result<T, E> {
    type ReadError = ResultReadError<T::ReadError, E::ReadError>;
    type WriteError = ResultWriteError<T::WriteError, E::WriteError>;

    fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
        Ok(match self {
            Ok(v) => {
                buf.push(false)?;
                v.write(buf).map_err(ResultWriteError::WriteOk)?
            }
            Err(v) => {
                buf.push(true)?;
                v.write(buf).map_err(ResultWriteError::WriteErr)?;
            }
        })
    }

    fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
        Ok(if buf.pop().ok_or(CopyError::Insufficient(Insufficient))? {
            Ok(T::read(buf).map_err(ResultReadError::ReadOk)?)
        } else {
            Err(E::read(buf).map_err(ResultReadError::ReadErr)?)
        })
    }
}

#[cfg(feature = "alloc")]
#[doc(inline)]
pub use _alloc::*;

#[cfg(feature = "alloc")]
mod _alloc {
    use super::*;

    use alloc::{
        string::{FromUtf8Error, String},
        vec,
        vec::Vec,
    };
    use bitbuf_vlq::{Error, Vlq};
    use core::convert::TryInto;

    #[derive(Debug)]
    pub enum StringReadError {
        Vlq(Error),
        TooLong,
        Utf8(FromUtf8Error),
        Buf(Insufficient),
    }

    impl From<FromUtf8Error> for StringReadError {
        fn from(input: FromUtf8Error) -> Self {
            StringReadError::Utf8(input)
        }
    }

    impl From<Error> for StringReadError {
        fn from(input: Error) -> Self {
            StringReadError::Vlq(input)
        }
    }

    impl From<Insufficient> for StringReadError {
        fn from(input: Insufficient) -> Self {
            StringReadError::Buf(input)
        }
    }

    impl MinCodec for String {
        type WriteError = CopyError;
        type ReadError = StringReadError;

        fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
            let bytes = self.as_bytes();
            buf.put_aligned(&*Vlq::from(bytes.len() as u64))?;
            buf.put_aligned(bytes)?;
            Ok(())
        }

        fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
            let len = Vlq::read(buf)?;
            let len: usize = len.try_into().map_err(|_| StringReadError::TooLong)?;
            let mut data = vec![0u8; len];
            buf.copy_aligned(&mut data)?;
            Ok(String::from_utf8(data)?)
        }
    }

    #[derive(Debug)]
    pub enum VecWriteError<T> {
        Content(T),
        Buf(Insufficient),
    }

    #[derive(Debug)]
    pub enum VecReadError<T> {
        Content(T),
        Vlq(Error),
        TooLong,
    }

    impl<T> From<Error> for VecReadError<T> {
        fn from(input: Error) -> Self {
            VecReadError::Vlq(input)
        }
    }

    impl<T> From<Insufficient> for VecWriteError<T> {
        fn from(input: Insufficient) -> Self {
            VecWriteError::Buf(input)
        }
    }

    impl<T: MinCodec> MinCodec for Vec<T> {
        type WriteError = VecWriteError<T::WriteError>;
        type ReadError = VecReadError<T::ReadError>;

        fn write<B: BitBufMut>(self, buf: &mut B) -> Result<(), Self::WriteError> {
            buf.put_aligned(&*Vlq::from(self.len() as u64))?;
            for item in self {
                item.write(buf).map_err(VecWriteError::Content)?;
            }
            Ok(())
        }

        fn read<B: BitBuf>(buf: &mut B) -> Result<Self, Self::ReadError> {
            let len = Vlq::read(buf)?;
            let len: usize = len.try_into().map_err(|_| VecReadError::TooLong)?;
            let mut data = Vec::with_capacity(len);
            for _ in 0..len {
                data.push(T::read(buf).map_err(VecReadError::Content)?)
            }
            Ok(data)
        }
    }
}
