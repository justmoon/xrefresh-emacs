# xrefresh.el

You're looking at an Emacs-based server for xrefresh. Use it to
automatically refresh your browser as you're developing in Emacs.

This obviously goes very well with Emacs' "never leave the home
row" philosophy. Just hit C-f C-s and watch your browser window
update.

# Feedback

Please send any questions, suggestions and other feedback to
[justmoon@members.fsf.org].

# Usage

1. Download xrefresh.el

2. Add the following to your dot-emacs:

       (require 'xrefresh)
       (xrefresh-start)

   For customizing your configuration, please see below.

3. Use the XRefresh browser plugin of your choice.

# Configuration

## How can I change the server port?

Simply pass the port as a parameter like so:

       (xrefresh-start 1337)

You can even start multiple servers on different ports if you so
desire. Not really sure what the point would be, but eh, you only live
once.

## What if my Emacs is running on a different machine than my browser?

*From [XRefresh FAQ](http://xrefresh.binaryage.com/#faq)*

XRefresh monitor communicates with browser extension using TCP/IP. So,
it is possible, but it may be tricky because you need to disable
firewall and make sure they see each other. By default browser
extension tries to connect to 127.0.0.1 on port 41258. In Firefox type
about:config into the URL bar and filter keys by "xrefresh". Keys
`extensions.xrefresh.host` and
`extensions.xrefresh.localConnectionsOnly` are what you are looking
for.

[TODO: Explain more configuration options.]

# Outstanding issues

* <del>Plugin doesn't transmit CSS file contents - so CSS live reload does
  not work.</del> **Works now**

Please report any issues to justmoon@members.fsf.org

# Thanks to

Dhruva Krishnamurthy - emacsserver.el

Binaryage - Makers of XRefresh
