#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (ps: [ps.HTTP ps.tagsoup])"
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03.tar.gz

import Network.HTTP
import Text.HTML.TagSoup

-- Fetch nixos.org and print all hrefs.
main = do
    resp <- Network.HTTP.simpleHTTP (getRequest "http://nixos.org/")
    body <- getResponseBody resp
    let tags = filter (isTagOpenName "a") $ parseTags body
    let tags' = map (fromAttrib "href") tags
    mapM_ putStrLn $ filter (/= "") tags'

