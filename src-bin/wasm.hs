import qualified Reflex.TodoMVC
import Language.Javascript.JSaddle.Wasm (run)

main :: IO ()
main = run 0 Reflex.TodoMVC.main
