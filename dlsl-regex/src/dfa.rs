use std::{
    array::from_fn,
    collections::{HashMap, hash_map::Entry},
};

use crate::{
    Nfa,
    nfa::{NdStateSet, TokenSet},
};

#[derive(Debug)]
pub struct State {
    pub transitions: [StateId; 256],
    pub tokens: TokenSet,
    pub accepting: TokenSet,
}

#[derive(Debug, Clone, Copy)]
pub struct StateId(usize);

#[derive(Debug)]
pub struct DfaCache {
    states: Vec<Option<State>>,
    id_of_states: HashMap<NdStateSet, StateId>,
    state_of_id: Vec<NdStateSet>,
}

impl DfaCache {
    pub fn id_of_states(&mut self, states: NdStateSet) -> StateId {
        match self.id_of_states.entry(states) {
            Entry::Occupied(occupied_entry) => *occupied_entry.get(),
            Entry::Vacant(vacant_entry) => {
                let id = StateId(self.states.len());
                vacant_entry.insert(id);
                self.states.push(None);
                id
            }
        }
    }

    pub fn get_state(&mut self, nfa: &Nfa, id: StateId) -> &State {
        if self.states[id.0].is_none() {
            let state = State {
                transitions: from_fn(|byte| {
                    self.id_of_states(
                        self.state_of_id[id.0]
                            .iter()
                            .map(|id| nfa[id].transitions[byte])
                            .collect(),
                    )
                }),
                tokens: nfa.tokens(self.state_of_id[id.0].iter()),
                accepting: nfa.accepting(self.state_of_id[id.0].iter()),
            };
            self.states[id.0] = Some(state);
        }
        self.states[id.0].as_ref().unwrap()
    }
}
