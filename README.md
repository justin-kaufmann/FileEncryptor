# FileEncryptor

__FileEncryptor allows encrypting file-contents with two passwords with an customized AES256-Encryption.__

## How it works

FileEncryptor encrypts / decrypts a file content with two passwords.  
The main part of the encryption/decryption will be done with the help of the Delphi Encryption Compendium.  
The 1st password is for encrypting/decrypting the file content.  
The 2nd password is for encrypting/decrypting the 1st password.

### After encryption

After encryption every encrypted file has an encrypted-marker within the file-content.  
This marker allows FileEncryptor to check whether a file is already encrypted or not.

### After decryption

After decryption the encrypted-marker gets deleted, so what remains is the original file content.

## Project-License

The code in this project is licensed under proprietary terms.  
This means that the modification, and distribution of the code are not permitted unless explicit written permission is granted by me.

## Usage

### How to encrypt a file

1. Type in the first password which decrypts the file-content.
2. Type in the second password which decrypts the first password.
3. Click on Encrypt.
4. Select a file.

### How to decrypt a file

1. Type in the first password which decrypts the file-content.
2. Type in the second password which decrypts the first password.
3. Click on Decrypt.
4. Select a file.

## Delphi Encryption Compendium

This project utilizes the Delphi Encryption Compendium, which is released under the Apache-2.0 License.  
The Delphi Encryption Compendium provides a variety of secure data encryption and decryption functionalities that have been implemented in this project.

### License

The implementation and use of the Delphi Encryption Compendium is done in accordance with the terms of the Apache-2.0 License.
The parts of the Delphi Encryption Compendium used in this project are copyrighted. 
All rights to this code and its associated documentation belong to the original author of the Delphi Encryption Compendium.
