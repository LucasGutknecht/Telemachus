import System.Environment
import System.Process
import System.FilePath (dropExtension, takeExtension)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when)

-- Melhorias a fazer: 1 - Corrigir a injeção de shell
-- Melhorias a fazer: 2 - Melhorar o tratamento de erros
-- Melhorias a fazer: 3 - Melhorar as mensagens de erros
-- Melhorias a fazer: 4 - Adicionar mais opcoes de flags do compilador
-- Melhorias a fazer: 5 - Adicionar a opcao do Lexer para produzir a lista tokens
-- Melhorias a fazer: 6 - Adicionar o parser

contemExtensaoC :: String -> Bool
contemExtensaoC s = takeExtension s == ".c"

montarComandoPreprocessar :: String -> IO String
montarComandoPreprocessar arquivo = do
    existe <- doesFileExist arquivo
    if not existe
        then error $ "Arquivo não existe: " ++ arquivo
        else return $ "gcc -E -P " ++ arquivo ++ " -o " ++ dropExtension arquivo ++ ".i"

montarComandoAssembly :: String -> IO String
montarComandoAssembly arquivo = do
    existe <- doesFileExist arquivo
    if not existe
        then error $ "Arquivo não existe: " ++ arquivo
        else return $ "gcc -S " ++ arquivo ++ " -o " ++ dropExtension arquivo ++ ".s"

montarComandoExecutavel :: String -> IO String
montarComandoExecutavel arquivo = do
    existe <- doesFileExist arquivo
    if not existe
        then error $ "Arquivo não existe: " ++ arquivo
        else return $ "gcc " ++ arquivo ++ " -o " ++ dropExtension arquivo

preprocessarArquivo :: String -> IO (Maybe String)
preprocessarArquivo arquivo = do
    comando <- montarComandoPreprocessar arquivo
    executa <- system comando
    case executa of
        ExitSuccess -> return Nothing
        ExitFailure _ -> return (Just "Erro no pré-processamento.")

compilarArquivoPreprocessado :: String -> IO (Maybe String)
compilarArquivoPreprocessado arquivo = do
    comando <- montarComandoAssembly (dropExtension arquivo ++ ".i")
    executa <- system comando
    case executa of
        ExitSuccess -> return Nothing
        ExitFailure _ -> return (Just "Erro na compilação de arquivo pré-processado.")

compilarArquivoExecutavel :: String -> IO (Maybe String)
compilarArquivoExecutavel arquivo = do
    comando <- montarComandoExecutavel (dropExtension arquivo ++ ".s")
    executa <- system comando
    case executa of
        ExitSuccess -> return Nothing
        ExitFailure _ -> return (Just "Erro na geração do executável.")

removerArquivosIntermediarios :: String -> IO ()
removerArquivosIntermediarios arquivo = do
    let arquivos = [dropExtension arquivo ++ ext | ext <- [".i", ".s"]]
    mapM_ removeFileIfExists arquivos

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists arquivo = do
    putStrLn $ "Tentando remover: " ++ arquivo
    existe <- doesFileExist arquivo
    if existe
        then do
            putStrLn $ "Arquivo existe, removendo: " ++ arquivo
            removeFile arquivo
            putStrLn $ "Arquivo removido: " ++ arquivo
        else
            putStrLn $ "Arquivo não encontrado: " ++ arquivo

removeFiles :: [FilePath] -> IO ()
removeFiles = mapM_ removeFileIfExists

executarEtapa :: IO (Maybe String) -> String -> IO Bool
executarEtapa acao mensagemSucesso = do
    resultado <- acao
    case resultado of
        Just erro -> do
            putStrLn erro
            return False
        Nothing -> do
            putStrLn mensagemSucesso
            return True

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Sem argumentos"
        (x:xs) -> do
            if not (contemExtensaoC x)
                then putStrLn $ "Erro: " ++ x ++ " não é um arquivo .c"
                else do
                    sucesso1 <- executarEtapa
                        (preprocessarArquivo x)
                        "Pré-processamento concluído com sucesso."

                    when sucesso1 $ do
                        sucesso2 <- executarEtapa
                            (compilarArquivoPreprocessado x)
                            "Assembly gerado com sucesso."

                        when sucesso2 $ do
                            _ <- executarEtapa
                                (compilarArquivoExecutavel x)
                                "Executável gerado com sucesso!"
                            removeFiles [dropExtension x ++ ".i", dropExtension x ++ ".s"]
                            putStrLn "Arquivos intermediários removidos com sucesso."
                            return ()
