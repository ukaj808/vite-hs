module Vite (
  collectUniqueCssChunks,
  collectUniqueImportedJsChunks,
  getEntryJsChunk,
  modulePreloadLinkTags,
  scriptTag,
  stylesheetLinkTags,
  viteHeadHtml,
  viteProxyMiddleware,
  CssChunk (..),
  EntryJsChunk (..),
  HtmlTag,
  ImportedJsChunk (..),
  JsChunk,
  ViteManifest (..),
  ViteProxySettings (..),
) where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe
import Network.Wai
import Network.Wai.Handler.WebSockets

import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Simple as Client
import qualified Network.WebSockets as WS
import qualified Wuss as WSS

type JsChunk = Either EntryJsChunk ImportedJsChunk
type BundledAsset = Either CssChunk JsChunk

newtype ViteManifest = ViteManifest {bundledAssets :: [BundledAsset]}

class HtmlTag a where
  tag :: a -> String

instance HtmlTag EntryJsChunk where
  tag ejsc =
    "<script type=\"module\" src=\""
      ++ entryJsChunkFilePath ejsc
      ++ "\"></script>"

instance HtmlTag ImportedJsChunk where
  tag ijsc =
    "<link rel=\"modulepreload\" href=\""
      ++ importedJsChunkFilePath ijsc
      ++ "\"/>"

instance HtmlTag CssChunk where
  tag cssc =
    "<link rel=\"stylesheet\" href=\""
      ++ cssChunkFilePath cssc
      ++ "\"/>"

-- Simplified Version for HTML
data EntryJsChunk = EntryJsChunk
  { entryJsChunkName :: String
  , entryJsChunkFilePath :: FilePath
  , entryJsChunkImports :: [ImportedJsChunk]
  , entryJsChunkCssImports :: [CssChunk]
  }
  deriving (Eq, Show)

data ImportedJsChunk = ImportedJsChunk
  { importedJsChunkFilePath :: FilePath
  , importedJsChunkImports :: [ImportedJsChunk]
  , importedJsChunkCssImports :: [CssChunk]
  }
  deriving (Eq, Show)

data CssChunk
  = CssChunk {cssChunkFilePath :: FilePath, cssChunkSrc :: String}
  | CssRef {cssChunkFilePath :: FilePath}
  deriving (Show)

instance Eq CssChunk where
  c1 == c2 =
    cssChunkFilePath c1 == cssChunkFilePath c2

instance FromJSON CssChunk where
  parseJSON (Object o) =
    CssChunk <$> o .: K.fromString "file" <*> o .: K.fromString "src"
  parseJSON (String t) =
    pure $ CssRef $ T.unpack t
  parseJSON invalid =
    prependFailure
      "parsing CssChunk failed, "
      (typeMismatch "Object" invalid)

instance FromJSON ViteManifest where
  parseJSON (Object o) =
    ViteManifest
      <$> sequence (foldParsers o)
   where
    foldParsers :: Object -> [Parser BundledAsset]
    foldParsers =
      KM.foldrWithKey
        ( \k v acc ->
            if ".css" `isSuffixOf` K.toString k
              then
                (Left <$> (parseJSON v :: Parser CssChunk)) : acc
              else
                ( Right
                    <$> withObject
                      "Either EntryJsChunk ImportedJsChunk"
                      ( \o'' ->
                          case KM.lookup (K.fromString "isEntry") o'' of
                            Nothing ->
                              Right
                                <$> ( ImportedJsChunk
                                        <$> o'' .: K.fromString "file"
                                        <*> parseImports o''
                                        <*> (o'' .:? K.fromString "css" .!= []) -- needs to be a key lookup instead
                                    )
                            Just _ ->
                              Left
                                <$> ( EntryJsChunk
                                        <$> (o'' .: K.fromString "name")
                                        <*> (o'' .: K.fromString "file")
                                        <*> parseImports o''
                                        <*> (o'' .:? K.fromString "css" .!= [])
                                    )
                      )
                      v
                )
                  : acc
        )
        []

    parseImport :: Value -> Parser ImportedJsChunk
    parseImport =
      withObject
        "ImportedJsChunk"
        ( \o' ->
            ImportedJsChunk
              <$> o' .: K.fromString "file"
              <*> parseImports o'
              <*> o' .:? K.fromString "css" .!= []
        )

    parseImports :: Object -> Parser [ImportedJsChunk]
    parseImports o' =
      case KM.lookup (K.fromString "imports") o' of
        Nothing -> pure []
        Just v ->
          V.toList
            <$> withArray
              "ImportedJsChunk"
              ( V.sequence
                  . V.map
                    ( withText
                        "ImportedJsChunk"
                        (\t -> maybe undefined parseImport (KM.lookup (K.fromText t) o))
                    )
              )
              v
  parseJSON invalid =
    prependFailure
      "parsing ViteManifest failed, "
      (typeMismatch "Object" invalid)

-- collect unique?
collectUniqueCssChunks :: EntryJsChunk -> [CssChunk]
collectUniqueCssChunks entryJsChunk =
  nub $
    entryJsChunkCssImports entryJsChunk
      ++ concatMap
        importedJsChunkCssImports
        (collectUniqueImportedJsChunks entryJsChunk)

collectUniqueImportedJsChunks :: EntryJsChunk -> [ImportedJsChunk]
collectUniqueImportedJsChunks entry =
  nub $ foldr (\i acc -> collect i ++ acc) [] (entryJsChunkImports entry)
 where
  collect :: ImportedJsChunk -> [ImportedJsChunk]
  collect i =
    i : foldr (\c acc -> collect c ++ acc) [] (importedJsChunkImports i)

getEntryJsChunk :: String -> ViteManifest -> Either String EntryJsChunk
getEntryJsChunk entryKey viteManifest =
  case possibleEntryJsChunk of
    Nothing -> Left $ "Entry Javascript chunk not found with key: " ++ entryKey
    Just entryJsChunk -> Right entryJsChunk
 where
  isEntryJsChunk (Left _) = Nothing
  isEntryJsChunk (Right (Right _)) = Nothing
  isEntryJsChunk (Right (Left entryJsChunk)) = Just entryJsChunk

  entryJsChunks =
    mapMaybe isEntryJsChunk $ bundledAssets viteManifest
  possibleEntryJsChunk =
    find (\c -> entryJsChunkName c == entryKey) entryJsChunks

scriptTag :: EntryJsChunk -> String
scriptTag = tag

stylesheetLinkTags :: EntryJsChunk -> [String]
stylesheetLinkTags entryJsChunk =
  map tag $ collectUniqueCssChunks entryJsChunk

modulePreloadLinkTags :: EntryJsChunk -> [String]
modulePreloadLinkTags entryJsChunk =
  map tag $ collectUniqueImportedJsChunks entryJsChunk

viteHeadHtml :: String -> ViteManifest -> Either String String
viteHeadHtml entryKey viteManifest = do
  entryJsChunk <- getEntryJsChunk entryKey viteManifest
  let stylesheets = stylesheetLinkTags entryJsChunk
      script = scriptTag entryJsChunk
      preloads = modulePreloadLinkTags entryJsChunk
  return $ unlines $ stylesheets ++ (script : preloads)

data ViteProxySettings = ViteProxySettings
  { proxyEnabled :: Bool
  , proxyPath :: [String]
  , viteServerPort :: Int
  , viteServerSecure :: Bool
  }

data WsClientSettings = WsClientSettings
  { wsClientHost :: String
  , wsClientPort :: Int
  , wsClientPath :: String
  , wsClientConnectionOptions :: WS.ConnectionOptions
  , wsClientHeaders :: WS.Headers
  , wsClientHandler :: WS.Connection -> IO ()
  }

viteProxyMiddleware :: ViteProxySettings -> Middleware
viteProxyMiddleware settings app req respond
  | not $ proxyEnabled settings = app req respond
  | not $
      proxyPath settings
        `isPrefixOf` map T.unpack (pathInfo req) =
      app req respond
  | otherwise = do
      case websocketsApp WS.defaultConnectionOptions hmr req of
        Just res -> do
          respond res
        Nothing -> do
          let viteReq =
                Client.setRequestHost
                  (UTF8.fromString "localhost")
                  $ Client.setRequestPort (fromIntegral $ viteServerPort settings)
                  $ Client.setRequestSecure (viteServerSecure settings)
                  $ Client.setRequestQueryString (queryString req)
                  $ Client.setRequestHeaders (requestHeaders req)
                  $ Client.setRequestMethod (requestMethod req)
                  $ Client.setRequestPath
                    (rawPathInfo req)
                    Client.defaultRequest
          viteRes <- Client.httpLBS viteReq
          respond $
            responseLBS
              (Client.getResponseStatus viteRes)
              (Client.getResponseHeaders viteRes)
              (Client.getResponseBody viteRes)
 where
  hmr :: WS.ServerApp
  hmr pc =
    if viteServerSecure settings
      then do
        WSS.runSecureClientWith
          (wsClientHost wsClientSettings)
          (read $ show $ wsClientPort wsClientSettings)
          (wsClientPath wsClientSettings)
          (wsClientConnectionOptions wsClientSettings)
          (wsClientHeaders wsClientSettings)
          (wsClientHandler wsClientSettings)
      else do
        WS.runClientWith
          (wsClientHost wsClientSettings)
          (wsClientPort wsClientSettings)
          (wsClientPath wsClientSettings)
          (wsClientConnectionOptions wsClientSettings)
          (wsClientHeaders wsClientSettings)
          (wsClientHandler wsClientSettings)
   where
    wsClientSettings :: WsClientSettings
    wsClientSettings =
      WsClientSettings
        { wsClientHost = "localhost"
        , wsClientPort = viteServerPort settings
        , wsClientPath = '/' : intercalate "/" (proxyPath settings) ++ "/"
        , wsClientConnectionOptions = WS.defaultConnectionOptions
        , wsClientHeaders = viteHmrHeaders
        , wsClientHandler =
            \viteServerConn -> do
              clientConn <- WS.acceptRequestWith pc WS.defaultAcceptRequest{WS.acceptSubprotocol = Just $ UTF8.fromString "vite-hmr"}
              print "Connection from client accepted!"
              WS.withPingThread viteServerConn 30 (return ()) $ viteMessageHandler viteServerConn clientConn
              WS.withPingThread clientConn 30 (return ()) $ clientMessageHandler clientConn viteServerConn
        }

    clientMessageHandler :: WS.Connection -> WS.Connection -> IO ()
    clientMessageHandler clientConn viteServerConn = do
      msg <- WS.receive clientConn
      WS.send viteServerConn msg
      clientMessageHandler clientConn viteServerConn

    viteMessageHandler :: WS.Connection -> WS.Connection -> IO ()
    viteMessageHandler viteServerConn clientConn = do
      msg <- WS.receive viteServerConn
      WS.send clientConn msg
      viteMessageHandler viteServerConn clientConn

    viteHmrHeaders :: WS.Headers
    viteHmrHeaders =
      [(CI.mk $ UTF8.fromString "Sec-WebSocket-Protocol", UTF8.fromString "vite-hmr")]
