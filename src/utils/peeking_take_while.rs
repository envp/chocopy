/// Implement a peek_while extension to peekable iterators.
///
/// This is essentially a peeking take_while, which doesn't consume the first
/// item to fail the predicate.
///
/// Taken from: https://stackoverflow.com/questions/69198757/create-peek-while-for-iterator#comment122316116_69198757
use std::iter::Peekable;

pub struct PeekingTakeWhile<I, P> {
    iter: I,
    predicate: P,
}

impl<I, P> Iterator for PeekingTakeWhile<&mut Peekable<I>, P>
where
    I: Iterator,
    P: Fn(&I::Item) -> bool,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<I::Item> {
        self.iter.next_if(&self.predicate)
    }
}

pub trait PeekingTakeWhileExt: Sized + Iterator {
    /// .
    fn peeking_take_while<P>(self, predicate: P) -> PeekingTakeWhile<Self, P>
    where
        P: Fn(&Self::Item) -> bool;
}

impl<I: Iterator> PeekingTakeWhileExt for &mut Peekable<I> {
    /// Behaves the same as `take_while` with the exception for the first
    /// element failing the predicate. `peeking_take_while` does not consume
    /// this token, unlike `take_while`.
    fn peeking_take_while<P>(self, predicate: P) -> PeekingTakeWhile<Self, P>
    where
        P: Fn(&I::Item) -> bool,
    {
        PeekingTakeWhile {
            iter: self,
            predicate,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::utils::peeking_take_while::PeekingTakeWhileExt;

    #[test]
    fn empty_input() {
        assert_eq!(
            std::iter::empty::<()>()
                .peekable()
                .peeking_take_while(|_| true)
                .collect::<Vec<()>>(),
            vec![]
        )
    }

    #[test]
    fn peek_while_string_chars() {
        let mut chars = "keyword:".chars().peekable();

        assert_eq!(
            chars
                .peeking_take_while(|c| c.is_alphabetic())
                .collect::<String>(),
            "keyword"
        );
        assert_eq!(chars.next().unwrap(), ':');
    }
}
