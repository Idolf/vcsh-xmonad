{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- Main inspiration: http://github.com/mortenbp/config
import XMonad
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop (desktopConfig)

----- Misc
import Prelude hiding (catch)
import Control.Exception(catch)
import Data.Monoid(mempty, mappend, All)
import Data.List((\\))
import Data.Ratio((%))
import Control.Monad(when)
import Control.Concurrent (threadDelay)
import System.Directory
import System.Locale
import System.Time
import System.Exit
import System.Process(system)
import Text.Regex.PCRE((=~))

----- Own packages
import XMonad.Actions.DynamicWorkspacesExtra
import XMonad.Hooks.UrgencyExtra
import XMonad.Layout.TopicExtra as TE
import XMonad.Layout.WorkspaceDirAlt
import XMonad.Util.ScratchpadAlt

----- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.WithAll
import XMonad.Actions.DynamicWorkspaces

----- Hooks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

----- Layout
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.MultiColumns

----- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile

----- Util
import XMonad.Util.Run
import XMonad.Util.EZConfig


main :: IO ()
main = checkTopicConfig myTopics myTopicConfig >> xmonad idolfConfig

myTerm :: String
myTerm = "sakura"

idolfConfig = withUrgencyHookC LibNotifyUrgencyHook urgencyConfig { remindWhen = Every 10 } $ ewmh $ desktopConfig
   { manageHook         = manageHook desktopConfig <+>
                          composeAll myManageHook <+>
                          scratchpadManageHook (W.RationalRect 0.1 0.1 0.8 0.8)
   , layoutHook         = smartBorders $ setWorkspaceDirs myLayout
   , terminal           = "exec " ++ myTerm
   , modMask            = mod4Mask
   , focusFollowsMouse  = False
   , handleEventHook    = myEventHook
   , logHook            = fadeOutLogHook $ fadeIf TE.isUnfocusedOnCurrentWS 0.8
   , borderWidth        = 0
   , workspaces         = myTopics
   , startupHook        = return () >> checkKeymap idolfConfig myKeys >> startupHook desktopConfig >> setWMName "LG3D"
   }
   `removeKeysP` (["M-q"] ++ ["M-" ++ m ++ k | m <- ["", "S-"], k <- map show [1..9 :: Int]])
   `additionalKeysP` myKeys


myLayout = onWorkspace "im" (withIM (1%5) (Role "buddy_list") Grid) $
           multiCol [1] 4 (3/100) (4/7) |||
           Full

myEventHook :: Event -> X All
myEventHook = deleteUnimportant (=~ "^(scratchpad|vm)-") callback
  where callback dead = withDir $ \tag dir ->
                  when (tag `elem` dead && tag =~ "^scratchpad-" && dir =~ ('^' : myScratchpadDir)) $ io $ deleteIfEmpty dir
        deleteIfEmpty dir = do contents <- getDirectoryContents dir
                               when (null $ contents \\ [".", ".."]) $ removeDirectory dir
                            `catch` \(_e :: IOError) -> return ()

myTopics :: [String]
myTopics =
  [ "yes"
  , "web"
  , "im"
  , "irc"
  , "organise"
  , "gmail"
  , "procrastination"
  , "virtualbox"
  , "wireshark"
  , "intel"
  , "ping"
  ]

setWorkspaceDirs layout =
    add "yes"               "~/yes"                      $
    workspaceDir "~" layout
  where add ws dir = onWorkspace ws (workspaceDir dir layout)

myManageHook :: [ManageHook]
myManageHook = [ className =? "Pidgin"          --> doShift "im"
               , className =? "VirtualBox"      -->
                 do name <- title
                    case (name =~ "( \\(.*\\))?( \\[[^\\]]+\\])? - Oracle VM VirtualBox$") :: (String,String,String) of
                     (_,"",_) -> return mempty
                     (n,_,_)  -> do let ws = "vm-" ++ n
                                    liftX (addHiddenWorkspace ws)
                                    doShift ws

               ]

myBrowser :: String
myBrowser = "chromium-browser"

shell :: X ()
shell = spawn (terminal idolfConfig)

browser, incogBrowser, newBrowser, appBrowser :: [String] -> X ()
browser         = safeSpawn myBrowser
incogBrowser s  = safeSpawn myBrowser ("--new-window" : "--incognito" : s)
newBrowser s    = safeSpawn myBrowser ("--new-window" : s)
appBrowser      = mapM_ (\s -> safeSpawn myBrowser ["--app=" ++ s])

myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
  { fgColor = "#a8a3f7"
  , bgColor = "#3f3c6d"
  , position = Top
  , promptBorderWidth = 0
  , font = "-misc-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
 }

myGSConfig :: HasColorizer a => GSConfig a
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , topicActions = M.fromList
      [ ("im", safeSpawn "pidgin" [])
      , ("yes", safeSpawn "emacs" ["yes.org"])
      , ("web", browser [])
      , ("irc", safeSpawn myTerm ["-e", "ssh", "lolbox.pwnies.dk", "-t", "screen", "-U", "-dR", "irc"])
      , ("organise", appBrowser ["https://calendar.google.com"])
      , ("gmail", appBrowser ["https://gmail.com"])
      , ("virtualbox", safeSpawn "virtualbox" [])
      , ("procrastination", newBrowser [ "https://cloud.feedly.com"
                                       , "http://reddit.com" ])
      , ("ping", safeSpawn myTerm ["-e", "ping", "8.8.8.8"])
      , ("wireshark", safeSpawn "wireshark" ["-k", "-i", "any"])
      ]
  , defaultTopicAction = const $ return ()
  , defaultTopic = "web"
  , maxTopicHistory = 10
  }

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-<Esc>", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ("M-S-q", io exitSuccess)
  , ("M-x", goToSelectedWS myTopicConfig True myGSConfig)
  , ("M-g", goToSelected myGSConfig)
  -- Lock
  , ("M-C-l", safeSpawn "slock" [])
  -- Application launcher
  , ("M-p", safeSpawn "rofi" ["-show", "run"])
  , ("M-C-p", safeSpawn "passmenu" [])
  , ("M-C-u", safeSpawn "passmenu" ["users"])
  -- Volume
  , ("<XF86AudioLowerVolume>", safeSpawn "/home/freaken/bin/volume" ["-5"])
  , ("<XF86AudioRaiseVolume>", safeSpawn "/home/freaken/bin/volume" ["+5"])
  , ("<XF86AudioMute>",        safeSpawn "/home/freaken/bin/volume" ["toggle"])
  -- Screen navigation
  , ("M-<Left>", prevScreen)
  , ("M-<Right>", nextScreen)
  , ("M-C-<Left>", shiftPrevScreen >> prevScreen)
  , ("M-C-<Right>", shiftNextScreen >> nextScreen)
  , ("M-<Up>", swapNextScreen)
  , ("M-<Down>", swapPrevScreen)
  , ("M-C-<Up>", swapNextScreen >> nextScreen)
  , ("M-C-<Down>", swapPrevScreen >> prevScreen)
  -- Wowser
  , ("M-b", browser [])
  , ("M-S-b", incogBrowser [])
  , ("M-o", toggleWS' ["NSP"])
  -- Dynamic workspaces
  , ("M-d", changeDir myXPConfig)
  , ("M-n", addWorkspacePrompt myXPConfig)
  , ("M-m", addWorkspaceMoveWindowPrompt myXPConfig)
  , ("M-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-r", renameWorkspace myXPConfig)
  , ("M-s", do dir <- liftIO $ formatCalendarTime defaultTimeLocale (myScratchpadDir ++ "/%Y-%m-%d-%H:%M:%S")  `fmap` (getClockTime >>= toCalendarTime)
               liftIO $ createDirectory dir
               newScratchpad
               changeDir_ dir
               shell)
  -- Workspace navigation
  , ("M-a", shiftToSelectedWS True myGSConfig)
  -- Scratchpad
  , ("M-S-<Space>", scratchpadSpawnActionCustom ("exec " ++ myTerm ++ " --class=scratchpad-window"))
  , ("M-S-g", toggleGlobal)
  -- Logging inspired by gwern
  , ("M-S-l", spawn ("date>>"++lg) >> appendFilePrompt myXPConfig lg)
  ]
  where lg = "/home/freaken/yes/2015-log.txt"
-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    W.StackSet {W.current = W.Screen { W.workspace = W.Workspace { W.tag = this } } } ->
      when (this `notElem` myTopics) removeWorkspace

myScratchpadDir :: String
myScratchpadDir = "/home/freaken/scratchpads"

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
