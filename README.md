# Cripto-Tool

**cripto-tool** é uma utilidade simples de terminal projetada para criptografar e descriptografar texto. Oferece uma interface fácil de usar para operações criptográficas básicas.

![Animação](https://github.com/user-attachments/assets/fafd8ff2-750a-4719-bdab-7087b0792684)


## Pontos positivos
- Criptografe e descriptografe texto com facilidade
- Interface de linha de comando simples
- Suporta múltiplos algoritmos de criptografia
- Leve e eficiente

## Como instalar?

Para Windows dentro da pasta installer terá o executavel ```cripto-tool-setup.exe``` basta rodar que ele abrirá o instalador.
Caso não for possivel concluir a instalação da forma convencional basta adicionar diretório que contem .exe dentro da pasta PATH.

## Comandos atualmente suportados

- encode-binary
- decode-binary
- encode-base64
- decode-base64
- caesar-encrypt
- caesar-decrypt
- rot13
- encode-hex
- decode-hex
- encode-MD5
- encode-MD4
- encode-MD2
- encode-SHA256
- encode-SHA512
- aes-encrypt
- aes-decrypt
- des-encrypt
- des-decrypt
- xor-encrypt
- xor-decrypt

Usar parametro ```-o nome_do_arquivo.txt``` para gerar um arquivo com resultado da operação, e utilizar ```-i``` para leitura de um arquivo ao invés de um texto diretamente no terminal.
Também parametro ```-k chave_de_criptografia``` deve ser utilizado para algoritmos que usam chave, caso não fornecido se utiliza "" como chave.


## Contribuindo
Contribuições são bem-vindas! Se você deseja adicionar recursos, corrigir erros ou melhorar a documentação, por favor, abra um problema ou envie um pull request.

## Licença
Este projeto está licenciado sob a Licença MIT. Veja o arquivo [LICENSE](LICENSE) para mais detalhes.
