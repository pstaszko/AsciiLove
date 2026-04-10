namespace ConsoleMVU.Gallery

module GalleryMessages =
    open GalleryDomain

    type GalleryMsg =
        | SetDisplayMode of ArtDisplayMode
        | SetFilter of ArtFilter
        | Next
        | Previous
        | Show
        | LoadArt
        | SetRandomArt
        | CheckForChanges
        | ClearArt
        | OpenImageText
        | ExploreImageText
        | OpenImageFileIfExists
        | DisplayArt of Art