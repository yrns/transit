use bevy_asset::{io::Reader, ron, *};
use bevy_reflect::TypePath;
use serde::Deserialize;
use thiserror::Error;

use crate::edit_egui as edit;

/// A transit edit graph asset.
#[derive(Asset, TypePath, Deserialize)]
#[serde(transparent)]
pub struct EditGraph(pub edit::Edit<super::Source>);

#[derive(Default)]
pub struct EditGraphLoader;

#[non_exhaustive]
#[derive(Debug, Error)]
pub enum Error {
    /// An [IO](std::io) Error.
    #[error("Could not load asset: {0}")]
    Io(#[from] std::io::Error),
    /// A [RON](ron) Error.
    #[error("Could not parse RON: {0}")]
    RonSpannedError(#[from] ron::error::SpannedError),
}

impl AssetLoader for EditGraphLoader {
    type Asset = EditGraph;
    type Settings = ();
    type Error = Error;

    async fn load<'a>(
        &'a self,
        reader: &'a mut Reader<'_>, // 0.14
        // reader: &'a mut dyn Reader, // 0.15
        _settings: &'a (),
        _load_context: &'a mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;
        let graph_asset = ron::de::from_bytes::<EditGraph>(&bytes)?;
        Ok(graph_asset)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}
