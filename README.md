# Email-to-Matrix tools

Even though E-mail is a very legacy transport, it is well supported in
many application servers. There barely exists any application without
support for sending notifications via e-mail.9

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

Version 1.6 or newer is required for `jq`. Otherwise `--rawfile`
switch doesn't work.

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

### Matrix room preparations

The bot doesn't automatically join on invite, currently. You need to
join the room using the bot. Remember to add room aliases you need.

### UniFi Video

Add an transport script to your email server. In Exim:

```
unifi-matrix : |/opt/email-to-matrix/unifi_parser
```

Ensure the script is executable and its configuration file is readable
by the mail user. For example in Debian and Exim, the user is
`Debian-exim`. Reload Exim configuration.

Enable email alerts from UniFi Video to send the email to that address on your server.

### Asterisk voicemail

We use Sangoma's FreeBPX distribution which comes with Exim but we
don't want to use full email server because it's easier to configure
ssmtp to send the email to our main server.

First, install ssmtp and optionally remove Exim. `yum install
ssmtp`. Then, set ssmtp as default agent with `update-alternatives
--config mta`.

Configure ssmtp by editing `/etc/ssmtp/ssmtp.conf`. The only option
you really need is `mailhub=ADDRESS` where *ADDRESS* is the IP or host
name of the upstream email server.

Add the transport script to your email server:

```
voicemail : |/opt/email-to-matrix/asterisk_voicemail_parser
```

Ensure the script is executable and its configuration file is readable
by the mail user. For example in Debian and Exim, the user is
`Debian-exim`. Reload Exim configuration.

Configure your voicemail account to send email to that email
address. Remember to tick option "Email attachment" because otherwise
the audio file is not there. If you want to receive the voice mail
only via Matrix, tick "Delete voicemail".

Add alias `#tele-EXTENSION@homeserver` to the room where you want to
receive the voice mail for given extension. For example in our
hackerspace our internal extension is 6906 so the alias should be
`#tele-6906:hacklab.fi`. The home server can be any server and can be
configured in `matrix.conf`.

## Security considerations

The script doesn't currently support encrypting the messages. So do
not use it on encrypted channels. Also, the email addresses used
should be kept private because the scripts are not validating where
the email is coming from. However, if you run the MTA on your
intranet, the issue is minor.
