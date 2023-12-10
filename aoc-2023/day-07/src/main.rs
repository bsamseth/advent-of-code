use aocd::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Card {
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl From<char> for Card {
    fn from(c: char) -> Self {
        match c {
            '*' => Card::Joker,
            '2' => Card::Two,
            '3' => Card::Three,
            '4' => Card::Four,
            '5' => Card::Five,
            '6' => Card::Six,
            '7' => Card::Seven,
            '8' => Card::Eight,
            '9' => Card::Nine,
            'T' => Card::Ten,
            'J' => Card::Jack,
            'Q' => Card::Queen,
            'K' => Card::King,
            'A' => Card::Ace,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
struct Hand {
    cards: [Card; 5],
    bid: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum HandKind {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

impl Hand {
    fn kind(&self) -> HandKind {
        let mut counts = [0; 14];
        for card in self.cards.iter() {
            counts[*card as usize] += 1;
        }

        let jokers = counts[Card::Joker as usize];
        counts[Card::Joker as usize] -= jokers;
        counts.sort_unstable();
        counts[13] += jokers;

        match counts[9..] {
            [_, _, _, _, 5] => HandKind::FiveOfAKind,
            [_, _, _, 1, 4] => HandKind::FourOfAKind,
            [_, _, _, 2, 3] => HandKind::FullHouse,
            [_, _, 1, 1, 3] => HandKind::ThreeOfAKind,
            [_, _, 1, 2, 2] => HandKind::TwoPair,
            [_, 1, 1, 1, 2] => HandKind::OnePair,
            _ => HandKind::HighCard,
        }
    }
}

fn winnings(hands: &mut [Hand]) -> u64 {
    hands.sort_unstable_by(|a, b| {
        let kind_a = a.kind();
        let kind_b = b.kind();
        if kind_a != kind_b {
            kind_a.cmp(&kind_b)
        } else {
            a.cards.cmp(&b.cards)
        }
    });
    hands
        .iter()
        .enumerate()
        .map(|(i, h)| (i as u64 + 1) * h.bid)
        .sum()
}

#[aocd(2023, 7)]
fn main() {
    let input = input!();

    let mut hands = input
        .lines()
        .map(|l| {
            let mut parts = l.split_whitespace();
            let cards = parts
                .next()
                .unwrap()
                .chars()
                .map(Card::from)
                .collect::<Vec<_>>();
            let bid = parts.next().unwrap().parse().unwrap();
            Hand {
                cards: [cards[0], cards[1], cards[2], cards[3], cards[4]],
                bid,
            }
        })
        .collect::<Vec<_>>();

    submit!(1, winnings(&mut hands));

    // Replace all Jacks with Jokers
    for hand in hands.iter_mut() {
        for card in hand.cards.iter_mut() {
            if *card == Card::Jack {
                *card = Card::Joker;
            }
        }
    }
    submit!(2, winnings(&mut hands));
}
