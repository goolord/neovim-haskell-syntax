import Neovim

import qualified Neovim.Plugin as P

main :: IO ()
main = do
    neovim defaultConfig
        { plugins = plugins defaultConfig ++ [ P.plugin ]
        }
