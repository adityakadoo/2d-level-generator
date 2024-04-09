module Main where

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Control.Monad (forever)
import System.Exit (exitSuccess)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)
import LoadShaders
import Graphics.GLUtil (readTexture, texture2DWrap)
import Text.Printf
import Data.Bifunctor ( Bifunctor(bimap) )
import System.Environment (getArgs)
import Randomness ( Seed )
import Data.List (transpose)
import Control.Monad.Random (RandomGen, mkStdGen, StdGen)
import WaveFuncCollapse
import Data.Word (Word8)
import TilesetLoader

data Descriptor =
     Descriptor BufferObject NumArrayIndices

data Context = Context {
  g :: StdGen,
  gridChoices :: [WaveFuncCollapse.Matrix Choices]
}

data GLMatrix a =
     GLMatrix !a !a !a !a
              !a !a !a !a
              !a !a !a !a
              !a !a !a !a
                deriving Eq

instance PrintfArg a => Show (GLMatrix a) where
  show :: PrintfArg a => GLMatrix a -> String
  show (GLMatrix m11 m12 m13 m14
                 m21 m22 m23 m24
                 m31 m32 m33 m34
                 m41 m42 m43 m44) =
    let matrix = "[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n\
                 \[ %v %v %v %v ]\n"
    in printf matrix m11 m12 m13 m14
                     m21 m22 m23 m24
                     m31 m32 m33 m34
                     m41 m42 m43 m44

cellSize :: Int
cellSize = 40

gridDim :: (Int, Int)
gridDim = (16,8)

genGrid :: RandomGen g => g -> Tileset -> Grid
genGrid g tileset = head (waveFuncCollapse g (tileIdx tileset) (neighbors tileset) (weights tileset) gridDim)

getVertices :: Int -> Grid -> [GLfloat]
getVertices nImgs grid = concatMap (tileToVert nImgs)
  (concat (enumerate2D (map reverse grid)))

enumerate2D :: [[a]] -> [[(Int, Int, a)]]
enumerate2D m = zipWith (\ i xs -> map (\ (j,x) -> (i,j,x)) xs) [0..] (map (zip [0..]) m)

tileToVert :: Int -> (Int, Int, Tile) -> [GLfloat]
tileToVert nImgs (x,y,tile)
  | tile == maxBound = getVert (x,y, fromIntegral tile, (0,0,0))
  | otherwise = getVert (x,y, t1, (((r `div` 2) + (r `mod` 2)) `mod` 2,
  r `div` 2, z))
  where
    t1 = fromIntegral tile `div` 8
    r = fromIntegral tile `mod` 4
    z = fromIntegral (tile `mod` 8) `div` 4
    getVert (x,y,t,(r1,r2,r3)) =
      [ -- | positions        -- | colors     -- | uv
        fI x+1.0, fI y+1.0,   col,   fI (t+1-r2)/fI nImgs, fI (r1*(1-r3) + r2*r3),
        fI x+1.0, fI y+0.0,   col,   fI (t+1-r1)/fI nImgs, fI ((1-r2)*(1-r3) + (1-r1)*r3),
        fI x+0.0, fI y+0.0,   col,   fI (t+r2)/fI nImgs, fI ((1-r1)*(1-r3) + (1-r2)*r3),
        fI x+0.0, fI y+1.0,   col,   fI (t+r1)/fI nImgs, fI (r2*(1-r3) + r1*r3)
      ]
      where
        col | t == fromIntegral (maxBound :: Tile) = 0.0
            | otherwise = 1.0

fI :: Int -> GLfloat
fI = fromIntegral

vertices :: RandomGen g => g -> Tileset -> [GLfloat]
vertices g tileset = getVertices (nImages tileset) (genGrid g tileset)

indices :: [GLuint]
indices = concatMap (\x -> map (+(4*x)) [
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]) [0..fromIntegral (uncurry (*) gridDim)]

keyPressed :: GLFW.KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

shutdown :: GLFW.WindowCloseCallback
shutdown win =
  do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
  do
    GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

openWindow :: String -> (Int, Int) -> IO GLFW.Window
openWindow title (sizex,sizey) =
  do
    GLFW.init
    GLFW.defaultWindowHints
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 4)
    GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 5)
    GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
    GLFW.windowHint (GLFW.WindowHint'Resizable False)
    Just win <- GLFW.createWindow sizex sizey title Nothing Nothing
    GLFW.makeContextCurrent (Just win)
    GLFW.setWindowSizeCallback win (Just resizeWindow)
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    return win

closeWindow :: GLFW.Window -> IO ()
closeWindow win =
  do
    GLFW.destroyWindow win
    GLFW.terminate

display :: StdGen -> String -> IO ()
display g tilesetName =
  do
    inWindow <- openWindow "2D Level Generator" (bimap (cellSize *) (cellSize *) gridDim)
    tileset <- fetchTileset tilesetName
    let context = ([initGrid (tileIdx tileset) gridDim], g)
    descriptor <- initResources (imgPath tileset)
      ((getVertices (nImages tileset).getFirstGrid.fst) context) indices
    -- descriptor <- initResources (imgPath tileset) (vertices g tileset) indices
    onDisplay inWindow descriptor context tileset
    closeWindow inWindow

-- onDisplay :: RandomGen g => Window -> Descriptor -> ([WaveFuncCollapse.Matrix Choices], g) -> IO a
onDisplay win descriptor@(Descriptor vertexBuffer numIndices) (gChoices, g) tileset =
  do
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]

    updateVBO ((getVertices (nImages tileset).getFirstGrid) gChoices) descriptor

    bindBuffer ArrayBuffer $= Just vertexBuffer
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
    GLFW.swapBuffers win

    forever $ do
       GLFW.pollEvents
       let newContext = waveFuncStep g (neighbors tileset) (weights tileset) gChoices
      --  let newContext = (gChoices, g)
       onDisplay win descriptor newContext tileset

-- | Init resources
---------------------------------------------------------------------------
updateVBO :: [GLfloat] -> Descriptor -> IO ()
updateVBO vs descriptor@(Descriptor vertexBuffer numIndices) =
  do
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    bindBuffer ArrayBuffer $= Nothing
    -- return $ Descriptor vertexBuffer numIndices


initResources :: String -> [GLfloat] -> [GLuint] -> IO Descriptor
initResources imgPath vs idx =
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length idx
    withArray idx $ \ptr ->
      do
        let idxSize = fromIntegral (numIndices * length idx)
        bufferData ElementArrayBuffer $= (idxSize, ptr, StaticDraw)

    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 5 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | Colors
    let vColor  = AttribLocation 1
        clrOffset  = 2 * floatSize
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor 1 Float stride (bufferOffset clrOffset))
    vertexAttribArray vColor   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 2
        uvOffset   = 3 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled


    -- | Assign Textures
    activeTexture            $= TextureUnit 0
    let tex_00 = imgPath
    tx0 <- loadTex tex_00
    texture Texture2D        $= Enabled
    textureBinding Texture2D $= Just tx0

    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "resources/shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "resources/shaders/shader.frag")]
    currentProgram $= Just program

    -- Set Uniforms
    location0 <- get (uniformLocation program "tex_00")
    uniform location0 $= TextureUnit 0

    -- Set Transform Matrix
    let tr =
          [ 1/fromIntegral (fst gridDim), 0, 0, 0
          , 0, 1/fromIntegral (snd gridDim), 0, 0
          , 0, 0, 1, 0
          , -0.5, -0.5, 0, 0.5 ] :: [GLfloat]

    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location1 <- get (uniformLocation program "transform")
    uniform location1 $= transform

    -- || Unload buffers
    -- bindVertexArrayObject         $= Nothing
    bindBuffer ArrayBuffer $= Nothing

    return $ Descriptor vertexBuffer (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return t
---------------------------------------------------------------------------

main :: IO ()
main =
  do
    args <- getArgs
    let seed = (read (head args) :: Seed)
    let tilesetName = args !! 1
    display (mkStdGen seed) tilesetName
