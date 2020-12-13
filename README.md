# Email-to-Matrix tools

Even though E-mail is very legacy transport, it is well supported in
many applications. There barely exists any applications without
support for sending notifications via e-mail.

Making Matrix pipes from all kinds of message passing facilities there
exists today would be so pointless work because the payload needs to
be decoded differently, anyway.

This repository has support for the following sources:

* [Unifi-video](https://video.ui.com/) by Ubiquiti
* [Asterisk voicemail](https://wiki.asterisk.org/wiki/display/AST/Voicemail)
* [Wordpress Contact Form 7](https://en.wordpress.org/plugins/contact-form-7/) by Takayuki Miyoshi

The principle is the same: Your email server's pipe transport runs the
command and the corresponding script parses the message and delivers
the message to Matrix.

You don't need your own Matrix homeserver but you need a mail
server. The mail server doesn't necessarily need to be open on the
Internet. You can have one in your local network where the services
are.

## Idea

On our hackerspace we have some Unifi surveillance cameras which send
email on motion. They send alerts by email which is pretty messy. This
script listens to incoming emails and tralates them to Matrix picture
messages.

Also, it suppresses sending messages if the alarm has been
disarmed. For that purpose we have an API endpoint where to check
arming status.

Finally, we had a need to deliver voice mail messages to our chat
room and also process our membership form data in GDPR compliant
fashion (i.e. without sending them to Google Mail).

## Requirements

```sh
sudo apt install jq uuid-runtime curl mpack
```

## Configuration

Copy template as your configuration file and set URLs, room name, and
Matrix access token.

```sh
cp matrix.conf.example matrix.conf
$EDITOR matrix.conf
```

Add pipe transport to your email server configuration. See
[Exim documentation of pipe transport](https://www.exim.org/exim-html-current/doc/html/spec_html/ch-the_pipe_transport.html)
for example. It works with variety of email servers but the examples
here are for Exim virtual domain configuration.

See your mail server documentation or make a little test script to
find out which user it is run by if you have any issues.

### UniFi Video

Add an transport script to your email server. In Exim:

```
unifi-matrix : |/opt/email-to-matrix/unifi_parser
```

Ensure the script is executable and its configuration file is readable
by the mail user. For example in Debian and Exim, the user is
`Debian-exim`.

Enable email alerts from UniFi Video to send the email to that address on your server.
