ocaml-ses
================
OCaml library for Amazon Simple Email Service (SES) => http://aws.amazon.com/ses/

Made to be integrated into a ocsigenserver project.

Installation
----------------
make && make install should do the trick

Dependencies
----------------
-  [Ocsigenserver](http://ocsigen.org/ocsigenserver/)
-  [Lwt](http://ocsigen.org/lwt/)
-  [Calendar](http://calendar.forge.ocamlcore.org/)
-  [Xmlm](http://erratique.ch/software/xmlm)

Usage
-----------------
All action and data type are implemented in as describe in ses api.

Check http://docs.amazonwebservices.com/ses/latest/APIReference/Welcome.html?r=5201 for more information.

    # Basic function to send a mail:
    let creds = {
      aws_key = "YOUR_AWS_KEY";
      aws_secret = "YOUR_AWS_SECRET";
    } in
    send_basic_email ~creds ~from_:"from@example.com" ~to_:"to@example.com" ~subject:"A subject" ~message_html:"A message" ()

All function raise 'Ses_error (type,code,message)' in case of error.
For type, code and message errros check http://docs.amazonwebservices.com/ses/latest/APIReference/CommonErrors.html (for common errors)
and http://docs.amazonwebservices.com/ses/latest/APIReference/API_Operations.html (for error specifiques to actions)

Authors
-----------------
Marc Simon

marc.simon42@gmail.com