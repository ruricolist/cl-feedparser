CL-FEEDPARSER is a Common Lisp port of a subset of Python’s
[feedparser][]. It does not do fetching; it only does parsing.

The API of cl-feedparser is very close to the API of feedparser. As in
Feedparser, the parsed feed is returned as a set of nested
dictionaries (hash tables). Unsurprisingly, keys that are strings in
Python are keywords in cl-feedparser, with dashes instead of
underscores.

    feed['author']        -> (feedparser:feed-ref feed :author)
    feed['author_detail'] -> (feedparser:feed-ref feed :author-detail)

(What is `feed-ref`? It is just like `gethash`, but if the provided
key is constant, then it does a compile-time check that the key
provided is valid. You can always just use `gethash`, of course.)

There are a few important differences:

- Parsed times are returned as universal times, rather than Python
  time tuples.
- The sanitizer strips all inline CSS.
- Text is always sanitized, regardless of declared media type. (Python
  feedparser does not sanitize `text/plain`.)
- `bozo-exception` is a list of exceptions.

In regard to sanitizing, cl-feedparser offers a guarantee Python’s
feedparser does not: values that are returned as strings are *always*
sanitized. It is possible to turn sanitizing off, but with sanitizing
off, instances of `unsanitized-string` are returned in place of
strings.

A warning: cl-feedparser is not nearly as sophisticated as Python
feedparser about handling timestamps. On the other hand, timestamps
are one area in which real-world feeds have actually improved since
feedparser was written.

CL-FEEDPARSER is a classic case of a [strangler application][].
Originally it was just a wrapper around the Python library; then
parsing well-formed feeds was moved into Lisp, with a fallback to
Python; finally, Lisp took over.

# EXAMPLE

``` lisp
(ql:quickload '(:cl-feedparser :drakma))

(defparameter *feed* (feedparser:parse-feed (drakma:http-request "http://planet.lisp.org/rss20.xml")))

(feedparser:feed-ref *feed* :title)
=> "Planet Lisp"
```

# FEEDBURNER

There is an extra key, `:proxy`, which is set to `"feedburner"` when
the feed is from Feedburner (or uses elements in the Feedburner
namespace, as Feedblitz does).

It is also the case that the Feedburner link is always overridden with
the feedburner:origLink. This is in the interest of future-proofing:
Feedburner may not be around forever, so for archival purposes you want the real links
instead of the Feedburner redirects.

# SKIPPING ENTRIES

In the wild, you will sometimes come across enormous feeds that have
simply never been truncated: they contain every post ever posted. And
even for ordinary feeds, you may only be interested in the latest
entry or two out of 10 or 20.

CL-FEEDPARSER lets you skip entries with two options: `:max-entries`
and `:guid-mask`.

If you know how many entries you want, pass `:max-entries`.

     (length (feed-ref (parse-feed giant-feed :max-entries 10) :entries))
     => 10

If you know which entries you *don’t* want (because you already have them), pass `guid-mask`.

Each entry of the mask can be either an id or an (id . timestamp)
pair. Any entry whose guid matches the supplied id is ignored. If the
timestamp is supplied, then the matching entry will not be ignored if
it has a timestamp that is newer than the timestamp on record. (That
is, it will still be ignored if it does not have a timestamp.)

It’s important to understand how the limit and the mask interact. The
limit takes effect first, and only then are the entries filtered with
the mask. This might seem strange, but think about a feed with
thousands of entries. If the mask were applied before the limit, you
would get another set of older entries every time you tried to parse
the feed.

# SANITIZING

By default, everything that might possibly be HTML is sanitized. This
includes cases where the media type is ambiguous; as such, information
might be lost. If that information is important, you may want to ask
`cl-feedparser` not to sanitize feeds.

There are actually two knobs: one turns off sanitizing for entry
contents, and the other turns it off for everything else (titles and
other metadata).

    ;; Don't sanitize entry contents.
    (parse-feed feed :sanitize-content nil)
    ;; Don’t sanitize titles or other metadata.
    (parse-feed feed :sanitize-title nil)

The idea here is that if you are going to be re-parsing the entry
contents anyway, you may want to skip the round-trip from string to
DOM and back to string again, and do the sanitizing more cheaply as
part of your own processing.

Turning off sanitizing has consequences. Any time `cl-feedparser`
returns a string, you may rely on it being sanitized. So, if you turn
off sanitizing, what you get are not strings, but instances of
`unsanitized-string`; to get the underlying string, you must call
`unsanitized-string-string` on them. (There is a type available,
`feed-string`, defined as `(or string unsanitized string)`, if you
want to do exhaustiveness checking.)

# HANDLING ERRORS

Since cl-feedparser builds on [FXML][] and [Plump][], it can can
understand and recover from many common XML errors. Sometimes,
however, you may not want automatic recovery. You might, for example,
want to validate a feed. In this case, it is enough to pass `:safe
nil` to `parse-feed`:

    (parse-feed buggy-feed :safe nil)
    => <ERROR>

Otherwise, errors during parsing are stored under the `:bozo-exception` key of the feed.

[feedparser]: https://pythonhosted.org/feedparser/
[TBRSS]: https://tbrss.com
[strangler application]: http://martinfowler.com/bliki/StranglerApplication.html
[FXML]: https://github.com/TBRSS/FXML
[Plump]: https://shinmera.github.io/plump
