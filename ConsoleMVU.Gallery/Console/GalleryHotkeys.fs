namespace ConsoleMVU.Gallery.Console

open ConsoleMVU.Gallery.GalleryMessages
open ConsoleMVU.Gallery.GalleryDomain

type GalleryHotkeyDef =
    {
        Key: string
        Description: string
        Message: GalleryMsg
    }

module GalleryHotkeys =
    let all : GalleryHotkeyDef list =
        [
            { Key = "+"; Description = ""; Message = Next }
            { Key = "-"; Description = ""; Message = Previous }
            { Key = "1"; Description = ""; Message = SetDisplayMode ArtDisplayMode.ImageIfAvailable }
            { Key = "2"; Description = ""; Message = SetDisplayMode ArtDisplayMode.Text }
            { Key = "3"; Description = ""; Message = SetFilter ArtFilter.All }
            { Key = "4"; Description = ""; Message = SetFilter ArtFilter.ImageOnly }
            { Key = "5"; Description = ""; Message = SetFilter ArtFilter.Newest }
            { Key = "t"; Description = ""; Message = OpenImageText }
            { Key = "e"; Description = ""; Message = ExploreImageText }
            { Key = "i"; Description = ""; Message = OpenImageFileIfExists }
        ]