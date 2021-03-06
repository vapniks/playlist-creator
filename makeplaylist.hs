#!/usr/local/bin/runhaskell
-- Script for creating .m3u playlists 
{-# LANGUAGE OverloadedStrings #-}

import           System.Exit
import           System.IO
import           Control.Exception
import           Control.Monad 
import           HsShellScript
import           System.FilePath 
import           System.Directory 
import           Data.Char

main :: IO ()
main =
   do let recurseopt = argdesc [ desc_short 'r', desc_long "recursive", 
                                 desc_description "Recursively read the subdirectories.", 
                                 desc_at_most_once ]
          relopt = argdesc [ desc_short 'R', desc_long "relative-paths",
                             desc_description "Use relative paths in the playlist.", 
                             desc_at_most_once ]
          nopromptopt = argdesc [ desc_short 'n', desc_long "noprompt", 
                                  desc_description "Don't prompt to overwrite existing playlist file."]
          appendopt = argdesc [ desc_short 'a', desc_long "append", 
                                desc_description "Append to existing playlist instead of overwriting it."]
          outputopt = argdesc [ desc_short 'o', desc_long "output", 
                                desc_description "Choose the name of the playlist file to generate (default behavior is to display on standard output).", 
                                desc_argname "FILE", desc_value_required, desc_at_most_once ]
          dirsopt = argdesc [ desc_direct, desc_description "File or directory containing music files", 
                              desc_argname "FILE/DIR", desc_at_least_once ]
          helpopt = argdesc [ desc_short 'h', desc_long "help", desc_description "Show help", desc_at_most_once ]
          header = "Usage: makeplaylist [-rRna] [-o FILE] FILE/DIR..."
      args <- getargs header [helpopt, recurseopt, relopt, nopromptopt, appendopt, outputopt, dirsopt]
      let recurse = arg_switch args recurseopt
          relpaths = arg_switch args relopt
          outopt = optarg_req args outputopt
          paths = args_req args dirsopt
          noprompt = arg_switch args nopromptopt
          append = arg_switch args appendopt
          help = arg_switch args helpopt
      -- Get the file handle/stdout
      hndl <- do case outopt of
                     Nothing -> return stdout
                     Just outfile -> do exists <- path_exists outfile 
                                        let basename = takeFileName outfile
                                            appendingStr = ("Appending to " ++ outfile)
                                            writingStr = ("Writing to " ++ outfile)
                                        -- prompt the user to overwrite or append to file if necessary
                                        mode <- if (exists && (not noprompt) && (not append))
                                                    then do yes <- yesnoPrompt (basename ++ " already exists, overwrite?")
                                                            if yes
                                                               then do putStrLn writingStr
                                                                       return WriteMode
                                                               else do append <- yesnoPrompt ("Append to " ++ basename ++ "?")
                                                                       if append
                                                                          then do putStrLn appendingStr
                                                                                  return AppendMode
                                                                          else exitFailure
                                                    else if append
                                                            then do putStrLn appendingStr
                                                                    return AppendMode
                                                            else do putStrLn writingStr
                                                                    return WriteMode
                                        openFile outfile mode
      if help
         then do putStrLn (usage_info args)
                 exitSuccess
         else return ()
      -- output directory
      outdir <- do case outopt of
                     Nothing -> getCurrentDirectory
                     Just outfile -> return (dir_part outfile)
      -- Get the list of files and write it to stdout/file
      allfiles <- ((listFiles paths recurse) >>= (filterM validFile))
      allpaths <- if relpaths
                     then (mapM (makeRelativePath outdir) allfiles)
                     else return allfiles
      mapM_ (hPutStrLn hndl) allpaths
      if outopt == Nothing
         then return ()
         else hClose hndl
      -- Catch any exceptions
      `catch`
         (\argerror -> do
             hPutStrLn stderr $ (argerror_message argerror) ++ "\n\n" ++ (argerror_usageinfo argerror)
             exitFailure)

-- Returns the common initial elements of two lists.
commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

-- Replace ~ with path to home directory in filepath argument
replaceTilde :: String -> IO String
replaceTilde str = if (head str) == '~'
                      then do home <- getHomeDirectory
                              return $ home ++ (drop 1 str)
                      else return str

-- Make a relative filepath from an absolute one.
-- The first arg is the absolute path to the base directory that the resulting path 
-- will be relative to, and the second arg is the absolute path to be made relative.
makeRelativePath :: String -> String -> IO String
makeRelativePath base path = do absBase <- replaceTilde base >>= realpath
                                absPath <- replaceTilde path >>= realpath
                                let baseparts = slice_path absBase
                                    pathparts = slice_path absPath
                                    commonparts = commonPrefix baseparts pathparts
                                    commonlen = length commonparts
                                    dotdots = take ((length baseparts) - commonlen) $ repeat ".."
                                return $ unslice_path (dotdots ++ (drop commonlen pathparts))

-- Returns a list of all files found in the paths listed in the first argument,
-- and their subdirectories. The second argument indicates whether or not to search
-- subdirectories recursively.
listFiles :: [String] -> Bool -> IO [String]
listFiles [] _ = return []
listFiles (x:xs) recurse = do isdir <- is_dir x
                              rest <- listFiles xs recurse
                              if isdir
                                 then if recurse
                                         then do cnts <- getDirectoryContents x
                                                 let nodots = (filter (flip notElem [".",".."]) cnts)
                                                     allfiles = (map (x </>) nodots)
                                                 a <- listFiles allfiles recurse
                                                 return (a ++ rest)
                                         else return rest
                                 else return (x:rest)

-- Check if a filepath exists and is valid
validFile :: String -> IO Bool
validFile file = do okpath <- path_exists file
                    let ext = takeExtension file
                        validExt = [".3ga",".3gp",".aac",".asf",".avi",".dat",".m2t",".mkv",".mov",".mp3",".mp4",".mpeg",".ogg",".vob",".webm",".wma",".wmv",".m4a"]
                    return (and [okpath,(elem ext ((map (map toUpper) validExt) ++ validExt))])

-- Prompt the user for a y/n answer
yesnoPrompt :: String -> IO Bool
yesnoPrompt msg = do putStrLn (msg ++ " (y or n)")
                     x <- getChar
                     if elem x ['y','Y'] then return True
                     else if elem x ['n','N'] then return False
                     else yesnoPrompt msg

