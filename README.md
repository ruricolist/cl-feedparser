CL-FEEDPARSER is a port of the feed-parsing part of Python’s
feedparser. It handles both well-formed and mildly invalid feeds.

There is an extra key, `:proxy`, which is set to `"feedburner"` when
the feed is from Feedburner (or uses elements in the Feedburner
namespace, like Feedblitz).

It is also the case that the Feedburner link is always overriden with
the feedburner:origLink.

This is the stub README.txt for the "cl-feedparser" project.

SKIPPING ENTRIES

In the wild, you will sometimes come across enormous feeds that have
simply never been truncated: they contain every post ever posted. And
even for ordinary feeds, you may only be interested in the latest
entry or two of 10 or 20.

CL-FEEDPARSER lets you skip entries with two options: `:max-entries`
and `:guid-mask`.

If you know how many entries you want, pass `:max-entries`.

     (length (@ (parse-feed feed :max-entries 10) :entries)) => 10

If you know which entries you *don’t* want (because you already have them), pass `guid-mask`.

     (notany (op (member ... )))

SANITIZING

By default, everything that might be HTML is sanitized. This includes
cases where the media type is ambiguous; as such, information might be
lost. It is, however, possible to ask `cl-feedparser` not to sanitize
feeds. There are actually two knobs: one turns off sanitizing for
entry contents, and the other turns it off for everything else (titles
and other metadata).

     (parse-feed-safe feed :sanitize-content nil)

The idea is that, if you are going to be parsing entry contents
anyway, you may want to skip the round-trip from string to DOM and
back to string again, and do the sanitizing more cheaply as part of
your own processing.

Turning off sanitizing has consequences. Any time `cl-feedparser`
returns a string, you may rely on it being sanitized. So, if you turn
off sanitizing, what you get are not strings, but instances of
`unsanitized-string`; to get the underlying string, you must call
`unsanitized-string-string` on them.
