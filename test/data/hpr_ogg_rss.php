<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0" xmlns:googleplay="http://www.google.com/schemas/play-podcasts/1.0" xmlns:atom="http://www.w3.org/2005/Atom" xmlns:itunes="http://www.itunes.com/dtds/podcast-1.0.dtd" >
<channel>
  <title>Hacker Public Radio</title>
  <link>http://hackerpublicradio.org/about.php</link>
  <itunes:subtitle>A daily show hosted the community on topics that are of interest to hackers and hobbyists.</itunes:subtitle>
  <description>Hacker Public Radio is an podcast that releases shows every weekday Monday through Friday. Our shows are produced by the community (you) and can be on any topic that are of interest to hackers and hobbyists.</description>
  <language>en-us</language>
  <itunes:category text="Technology">
    <itunes:category text="Tech News"/>
  </itunes:category>
  <itunes:category text="Education">
    <itunes:category text="Training"/>
  </itunes:category>
  <itunes:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
  <itunes:explicit>yes</itunes:explicit>
  <itunes:keywords>Community Radio, Tech Interviews, Linux, Open, Hobby, Software Freedom</itunes:keywords>
  <copyright>Creative Commons Attribution-ShareAlike 3.0 License</copyright>
  <managingEditor>feedback@NOSPAM-hackerpublicradio.org (HPR Feedback)</managingEditor> 
<!--   <author>feedback@NOSPAM-hackerpublicradio.org (HPR Feedback)</author> -->
  <itunes:owner>    
    <itunes:name>HPR Volunteer</itunes:name>
    <itunes:email>admin@hackerpublicradio.org</itunes:email>
  </itunes:owner>
  <webMaster>admin@hackerpublicradio.org (HPR Volunteer)</webMaster> 
  <generator>kate</generator> 
  <docs>http://www.rssboard.org/rss-specification</docs>
  <ttl>43200</ttl>
  <skipDays>
    <day>Saturday</day>
    <day>Sunday</day>
  </skipDays>
  <image>
    <url>http://hackerpublicradio.org/images/hpr_feed_small.png</url>
    <title>Hacker Public Radio</title>
    <link>http://hackerpublicradio.org/about.php</link>
    <description>The Hacker Public Radio Old Microphone Logo</description>
    <height>164</height>
    <width>144</width>
  </image>
  <googleplay:author>HPR Volunteer</googleplay:author>
  <googleplay:description>Hacker Public Radio is an podcast that releases shows every weekday Monday through Friday. Our shows are produced by the community (you) and can be on any topic that are of interest to hackers and hobbyists.</googleplay:description>
  <googleplay:email>admin@hackerpublicradio.org</googleplay:email>
  <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
  <googleplay:category text="Technology"/>
  <atom:link href="http://hackerpublicradio.org/hpr_ogg_rss.php" rel="self" type="application/rss+xml" />
  <pubDate>Sat, 01 Jun 2019 00:00:00 +0000</pubDate>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2825: More text to speech trials</title>
    <author>ken.nospam@nospam.fallon.ie (Ken Fallon)</author>
    <googleplay:author>ken.nospam@nospam.fallon.ie (Ken Fallon)</googleplay:author>
    <itunes:author>ken.nospam@nospam.fallon.ie (Ken Fallon)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2825</link>
    <description><![CDATA[<p>
A supplementary show to Jeroens episode <a href="http://hackerpublicradio.org/eps.php?id=2792">HPR2792 :: Playing around with text to speech synthesis on Linux</a>.
</p>
<p>
I found two addional options. The first is <a href="https://mimic.mycroft.ai/">mimic</a>
</p>
<pre>
# dnf info mimic
Summary      : Mycroft's TTS engine
URL          : https://mimic.mycroft.ai/
License      : BSD
Description  : Mimic is a fast, lightweight Text-to-speech engine developed by Mycroft A.I.
             : and VocalID, based on Carnegie Mellon University’s FLITE software. Mimic takes
             : in text and reads it out loud to create a high quality voice. Mimic's
             : low-latency, small resource footprint, and good quality voices set it apart
             : from other open source text-to-speech projects.
</pre>

<p>
And the second is <a href="https://gtts.readthedocs.io/en/latest/index.html">gTTS</a> which is a interface to the google TTS api.
</p>
]]>
</description>
    <itunes:summary><![CDATA[
A supplementary show to Jeroens episode HPR2792 :: Playing around with text to speech synthesis on Linux.


I found two addional options. The first is mimic


# dnf info mimic
Summary      : Mycroft's TTS engine
URL          : https://mimic.mycroft.ai/
License      : BSD
Description  : Mimic is a fast, lightweight Text-to-speech engine developed by Mycroft A.I.
             : and VocalID, based on Carnegie Mellon University’s FLITE software. Mimic takes
             : in text and reads it out loud to create a high quality voice. Mimic's
             : low-latency, small resource footprint, and good quality voices set it apart
             : from other open source text-to-speech projects.



And the second is gTTS which is a interface to the google TTS api.

]]>
</itunes:summary>
    <pubDate>Fri, 31 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2825.ogg" length="3647471" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2825.ogg</guid>
  </item>
  <item>
    <itunes:explicit>yes</itunes:explicit>
    <googleplay:explicit>Yes</googleplay:explicit>
    <title>HPR2824: Gnu Awk - Part 15</title>
    <author>perloid.nospam@nospam.autistici.org (Dave Morriss)</author>
    <googleplay:author>perloid.nospam@nospam.autistici.org (Dave Morriss)</googleplay:author>
    <itunes:author>perloid.nospam@nospam.autistici.org (Dave Morriss)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2824</link>
    <description><![CDATA[<h2 id="introduction">Introduction</h2>
<p>This is the fifteenth episode of the “<a href="http://hackerpublicradio.org/series.php?id=94" title="Learning Awk">Learning Awk</a>” series which is being produced by <a href="http://hackerpublicradio.org/correspondents.php?hostid=300" title="b-yeezi">b-yeezi</a> and myself.</p>
<p>This is the second of a pair of episodes looking at <em>redirection</em> in Awk scripts.</p>
<p>In this episode I will spend some time looking at the <code>getline</code> command used for <em>explicit input</em> (as opposed to the usual <em>implicit</em> sort), often with redirection. The <code>getline</code> command is a complex subject which I will cover only relatively briefly. You are directed to the <a href="https://www.gnu.org/software/gawk/manual/gawk.html#Getline" title="Explicit Input with getline"><code>getline</code> section</a> of the GNU Awk User’s Guide for the full details.</p>
<h2 id="long-notes">Long notes</h2>
<p>I have provided detailed notes as usual for this episode, and these can be <a href="http://hackerpublicradio.org/eps/hpr2824/full_shownotes.html">viewed here</a>.</p>
<h2 id="links">Links</h2>
<ul>
<li><a href="https://www.gnu.org/software/gawk/manual/html_node/index.html"><em>GNU Awk User’s Guide</em></a>
<ul>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Getline">Explicit Input with <code>getline</code></a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Plain-Getline">Using <code>getline</code> with No Arguments</a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Getline_002fCoprocess">Using <code>getline</code> with a <em>coprocess</em></a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Getline-Notes">Getline notes</a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Two_002dway-I_002fO">Two-way I/O</a></li>
</ul></li>
</ul>
<ul>
<li>Previous shows in this series on HPR:
<ul>
<li><a href="http://hackerpublicradio.org/eps.php?id=2114">“<em>Gnu Awk - Part 1</em>”</a> - episode 2114</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2129">“<em>Gnu Awk - Part 2</em>”</a> - episode 2129</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2143">“<em>Gnu Awk - Part 3</em>”</a> - episode 2143</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2163">“<em>Gnu Awk - Part 4</em>”</a> - episode 2163</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2184">“<em>Gnu Awk - Part 5</em>”</a> - episode 2184</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2238">“<em>Gnu Awk - Part 6</em>”</a> - episode 2238</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2330">“<em>Gnu Awk - Part 7</em>”</a> - episode 2330</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2438">“<em>Gnu Awk - Part 8</em>”</a> - episode 2438</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2476">“<em>Gnu Awk - Part 9</em>”</a> - episode 2476</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2526">“<em>Gnu Awk - Part 10</em>”</a> - episode 2526</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2554">“<em>Gnu Awk - Part 11</em>”</a> - episode 2554</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2610">“<em>Gnu Awk - Part 12</em>”</a> - episode 2610</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2804">“<em>Gnu Awk - Part 13</em>”</a> - episode 2804</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2816">“<em>Gnu Awk - Part 14</em>”</a> - episode 2816</li>
</ul></li>
<li>Resources:
<ul>
<li><a href="http://hackerpublicradio.org/eps/hpr2824/full_shownotes.epub">ePub version of these notes</a></li>
<li>Examples: <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_testdata1">awk15_testdata1</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex1.awk">awk15_ex1.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex2.awk">awk15_ex2.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_testdata2">awk15_testdata2</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex3.awk">awk15_ex3.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex4.awk">awk15_ex4.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex5.awk">awk15_ex5.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex6.awk">awk15_ex6.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2824/awk15_ex7.awk">awk15_ex7.awk</a></li>
</ul></li>
</ul>
]]>
</description>
    <itunes:summary><![CDATA[Introduction
This is the fifteenth episode of the “Learning Awk” series which is being produced by b-yeezi and myself.
This is the second of a pair of episodes looking at redirection in Awk scripts.
In this episode I will spend some time looking at the getline command used for explicit input (as opposed to the usual implicit sort), often with redirection. The getline command is a complex subject which I will cover only relatively briefly. You are directed to the getline section of the GNU Awk User’s Guide for the full details.
Long notes
I have provided detailed notes as usual for this episode, and these can be viewed here.
Links

GNU Awk User’s Guide

Explicit Input with getline
Using getline with No Arguments
Using getline with a coprocess
Getline notes
Two-way I/O



Previous shows in this series on HPR:

“Gnu Awk - Part 1” - episode 2114
“Gnu Awk - Part 2” - episode 2129
“Gnu Awk - Part 3” - episode 2143
“Gnu Awk - Part 4” - episode 2163
“Gnu Awk - Part 5” - episode 2184
“Gnu Awk - Part 6” - episode 2238
“Gnu Awk - Part 7” - episode 2330
“Gnu Awk - Part 8” - episode 2438
“Gnu Awk - Part 9” - episode 2476
“Gnu Awk - Part 10” - episode 2526
“Gnu Awk - Part 11” - episode 2554
“Gnu Awk - Part 12” - episode 2610
“Gnu Awk - Part 13” - episode 2804
“Gnu Awk - Part 14” - episode 2816

Resources:

ePub version of these notes
Examples: awk15_testdata1, awk15_ex1.awk, ]]>
</itunes:summary>
    <pubDate>Thu, 30 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2824.ogg" length="18042197" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2824.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2823: Gentoo and why I use it</title>
    <author>alden.peeters.nospam@nospam.leagueh.xyz (aldenp)</author>
    <googleplay:author>alden.peeters.nospam@nospam.leagueh.xyz (aldenp)</googleplay:author>
    <itunes:author>alden.peeters.nospam@nospam.leagueh.xyz (aldenp)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2823</link>
    <description><![CDATA[<p>Thanks to <a href="http://hackerpublicradio.org/correspondents.php?hostid=342">norrist</a> for suggesting I do this episode!</p>
<ul>
<li><a href="https://gentoo.org/" class="uri">https://gentoo.org/</a></li>
</ul>
]]>
</description>
    <itunes:summary><![CDATA[Thanks to norrist for suggesting I do this episode!

https://gentoo.org/

]]>
</itunes:summary>
    <pubDate>Wed, 29 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2823.ogg" length="8350574" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2823.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2822: What's in the Box! Part 1</title>
    <author>nybill.nospam@nospam.gunmonkeynet.net (NYbill)</author>
    <googleplay:author>nybill.nospam@nospam.gunmonkeynet.net (NYbill)</googleplay:author>
    <itunes:author>nybill.nospam@nospam.gunmonkeynet.net (NYbill)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2822</link>
    <description><![CDATA[<p>NYbill opens a mystery box that arrived in the mail.</p>
<p>No spoilers. But, it involves soldering…</p>
<p>Pics for the episode:</p>
<p><a href="http://media.gunmonkeynet.net/u/nybill/collection/what-s-in-the-box/" class="uri">http://media.gunmonkeynet.net/u/nybill/collection/what-s-in-the-box/</a></p>]]>
</description>
    <itunes:summary><![CDATA[NYbill opens a mystery box that arrived in the mail.
No spoilers. But, it involves soldering…
Pics for the episode:
http://media.gunmonkeynet.net/u/nybill/collection/what-s-in-the-box/]]>
</itunes:summary>
    <pubDate>Tue, 28 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2822.ogg" length="10621582" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2822.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2821: Interviewing some exhibitors at the 2019 vcfe.org event</title>
    <author>jbaten.nospam@nospam.i2rs.nl (Jeroen Baten)</author>
    <googleplay:author>jbaten.nospam@nospam.i2rs.nl (Jeroen Baten)</googleplay:author>
    <itunes:author>jbaten.nospam@nospam.i2rs.nl (Jeroen Baten)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2821</link>
    <description><![CDATA[<p>I visited the vcfe.org event in Munich, Germany.</p>
<p>Below you will find some urls for the projects that I came across.</p>
<ul>
<li>Steckschwein 6502 computer: <a href="https://steckschwein.de/" class="uri">https://steckschwein.de/</a></li>
<li>Siemens Simatic S5 PLC: <a href="https://en.wikipedia.org/wiki/Simatic_S5_PLC" class="uri">https://en.wikipedia.org/wiki/Simatic_S5_PLC</a></li>
<li>CP/M operating system: <a href="https://en.wikipedia.org/wiki/CP/M" class="uri">https://en.wikipedia.org/wiki/CP/M</a></li>
<li>Old fashioned MUD game Nemesis: <a href="http://nemesis.de" class="uri">http://nemesis.de</a></li>
<li>Eastern Germany Robotron hardware: <a href="https://en.wikipedia.org/wiki/VEB_Robotron" class="uri">https://en.wikipedia.org/wiki/VEB_Robotron</a></li>
</ul>
<p>If you like these things, the next exhibition will be in September in Berlin (you can find more info on <a href="http://vcfb.de/2019/">vcfb.de</a>).</p>
<p>Regards, Jeroen Baten</p>]]>
</description>
    <itunes:summary><![CDATA[I visited the vcfe.org event in Munich, Germany.
Below you will find some urls for the projects that I came across.

Steckschwein 6502 computer: https://steckschwein.de/
Siemens Simatic S5 PLC: https://en.wikipedia.org/wiki/Simatic_S5_PLC
CP/M operating system: https://en.wikipedia.org/wiki/CP/M
Old fashioned MUD game Nemesis: http://nemesis.de
Eastern Germany Robotron hardware: https://en.wikipedia.org/wiki/VEB_Robotron

If you like these things, the next exhibition will be in September in Berlin (you can find more info on vcfb.de).
Regards, Jeroen Baten]]>
</itunes:summary>
    <pubDate>Mon, 27 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2821.ogg" length="25427327" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2821.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2820: 29 - CERT Home Security Tips</title>
    <author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</author>
    <googleplay:author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</googleplay:author>
    <itunes:author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2820</link>
    <description><![CDATA[<p>The Computer Emergency Readiness Team of the US Department of Homeland Security issues a security bulletin, ST15-002, which has tips for home network security. In this episode we review these tips and why they make sense.</p>
<h2 id="links">Links:</h2>
<ul>
<li><a href="https://www.us-cert.gov/ncas/tips/ST15-002" class="uri">https://www.us-cert.gov/ncas/tips/ST15-002</a></li>
<li><a href="http://www.zwilnik.com/" class="uri">http://www.zwilnik.com/</a></li>
</ul>
]]>
</description>
    <itunes:summary><![CDATA[The Computer Emergency Readiness Team of the US Department of Homeland Security issues a security bulletin, ST15-002, which has tips for home network security. In this episode we review these tips and why they make sense.
Links:

https://www.us-cert.gov/ncas/tips/ST15-002
http://www.zwilnik.com/

]]>
</itunes:summary>
    <pubDate>Fri, 24 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2820.ogg" length="13615473" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2820.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2819: Reply to Knightwise - podcasts</title>
    <author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</author>
    <googleplay:author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</googleplay:author>
    <itunes:author>zwilnik.nospam@nospam.zwilnik.com (Ahuka)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2819</link>
    <description><![CDATA[<p>Knightwise, in <a href="http://hackerpublicradio.org/eps.php?id=2798">HPR 2798</a>, made the argument that podcasts are better if they are done by "pirates", i.e. not by corporations, but by individuals with something to say. While I see some merit in this view, I think the more significant feature of podcasts is that it gets us away from "broadcasting" (shows aimed at the lowest common denominator) and towards "narrowcasting", an environment where small niche interests can find an audience and thrive since podcasting does not require a lot of resources. But I do appreciate the chance to hear some radio programs that I would not otherwise be able to listen to when they are offered as podcasts.</p>
<h2 id="links">Links:</h2>
<ul>
<li><a href="https://www.npr.org/podcasts/583350334/science-friday" class="uri">https://www.npr.org/podcasts/583350334/science-friday</a></li>
<li><a href="https://www.bbc.co.uk/programmes/b00snr0w" class="uri">https://www.bbc.co.uk/programmes/b00snr0w</a></li>
<li><a href="http://www.palain.com" class="uri">http://www.palain.com</a></li>
</ul>]]>
</description>
    <itunes:summary><![CDATA[Knightwise, in HPR 2798, made the argument that podcasts are better if they are done by &quot;pirates&quot;, i.e. not by corporations, but by individuals with something to say. While I see some merit in this view, I think the more significant feature of podcasts is that it gets us away from &quot;broadcasting&quot; (shows aimed at the lowest common denominator) and towards &quot;narrowcasting&quot;, an environment where small niche interests can find an audience and thrive since podcasting does not require a lot of resources. But I do appreciate the chance to hear some radio programs that I would not otherwise be able to listen to when they are offered as podcasts.
Links:

https://www.npr.org/podcasts/583350334/science-friday
https://www.bbc.co.uk/programmes/b00snr0w
http://www.palain.com
]]>
</itunes:summary>
    <pubDate>Thu, 23 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2819.ogg" length="5887605" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2819.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2818: Writing Web Game in Haskell - Science, part 1</title>
    <author>tuukka.turto.nospam@nospam.oktaeder.net (tuturto)</author>
    <googleplay:author>tuukka.turto.nospam@nospam.oktaeder.net (tuturto)</googleplay:author>
    <itunes:author>tuukka.turto.nospam@nospam.oktaeder.net (tuturto)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2818</link>
    <description><![CDATA[<h2 id="background">Background</h2>
<p>This is rather large topic, so I split it in two episodes. Next one should follow in two weeks if everything goes as planned. First part is about modeling research, while second part concentrates on how things change over time.</p>
<p>There’s three types of research: engineering, natural sciences and social sciences. Research costs points that are produced by various buildings.</p>
<h2 id="implementation">Implementation</h2>
<p>There’s three database tables, which are defined below:</p>
<pre><code>CurrentResearch
    type Technology
    progress Int
    factionId FactionId

AvailableResearch
    type Technology
    category TopResearchCategory
    factionId FactionId

CompletedResearch
    type Technology
    level Int
    factionId FactionId
    date Int</code></pre>
<h2 id="data-types">Data types</h2>
<p><code>Technology</code> is enumeration of all possible technologies. Knowing these enable player to build specific buildings and space ships, enact various laws and so on. In the end this will be (hopefully) large list of technologies.</p>
<pre><code>data Technology =
    HighSensitivitySensors
    | SideChannelSensors
    | HighTensileMaterials
    | SatelliteTechnology
    | BawleyHulls
    | SchoonerHulls
    | CaravelHulls
    ...
    deriving (Show, Read, Eq, Enum, Bounded, Ord)</code></pre>
<p>All research belong to one of the top categories that are shown below:</p>
<pre><code>data TopResearchCategory =
    Eng
    | NatSci
    | SocSci
    deriving (Show, Read, Eq, Ord)</code></pre>
<p><code>ResearchCategory</code> is more fine grained division of research. Each of the categories is further divided into sub-categories. Only <code>EngineeringSubField</code> is shown below, but other two are similarly divided.</p>
<pre><code>data ResearchCategory =
    Engineering EngineeringSubField
    | NaturalScience NaturalScienceSubField
    | SocialScience SocialScienceSubField
    deriving (Show, Read, Eq)

data EngineeringSubField =
    Industry
    | Materials
    | Propulsion
    | FieldManipulation
    deriving (Show, Read, Eq)</code></pre>
<p><code>ResearchScore</code> is measure of how big some research is. It has type parameter <code>a</code> that is used to further quantify what kind of <code>ResearchScore</code> we’re talking about.</p>
<pre><code>newtype ResearchScore a = ResearchScore { unResearchScore :: Int }
    deriving (Show, Read, Eq, Ord, Num)</code></pre>
<p><code>TotalResearchScore</code> is record of three different types of researches. I’m not sure if I should keep it as a record of three fields or if I should change it so that only one of those values can be present at any given time.</p>
<pre><code>data TotalResearchScore a = TotalResearchScore
    { totalResearchScoreEngineering :: ResearchScore EngineeringCost
    , totalResearchScoreNatural :: ResearchScore NaturalScienceCost
    , totalResearchScoreSocial :: ResearchScore SocialScienceCost
    }
    deriving (Show, Read, Eq)</code></pre>
<p>Following singleton values are used with <code>ResearchScore</code> and <code>TotalResearchScore</code> to quantify what kind of value we’re talking about.</p>
<pre><code>data EngineeringCost = EngineeringCost
    deriving (Show, Read, Eq)

data NaturalScienceCost = NaturalScienceCost
    deriving (Show, Read, Eq)

data SocialScienceCost = SocialScienceCost
    deriving (Show, Read, Eq)

data ResearchCost = ResearchCost
    deriving (Show, Read, Eq)

data ResearchProduction = ResearchProduction
    deriving (Show, Read, Eq)

data ResearchLeft = ResearchLeft
    deriving (Show, Read, Eq)</code></pre>
<p>Finally there’s <code>Research</code>, which is a record that uses many of the types introduced earlier. It describes what <code>Technology</code> is unlocked upon completion, what’s the cost is and if there are any technologies that have to have been researched before this research can start. The tier of research isn’t currently used for anything, but I have vague plans what to do about it in the future.</p>
<pre><code>data Research = Research
    { researchName :: Text
    , researchType :: Technology
    , researchCategory :: ResearchCategory
    , researchAntecedents :: [Technology]
    , researchCost :: TotalResearchScore ResearchCost
    , researchTier :: ResearchTier
    }
    deriving (Show, Read, Eq)</code></pre>
<h2 id="tech-tree">Tech tree</h2>
<p>Putting all this together, we can define a list of <code>Research</code>. Since finding an entry from this list based on research type of it is such a common operation, we also define another data structure for this specific purpose. <code>Map</code> in other programming languages is often known as dictionary, associative array or hash map. It stores key-value - pairs. In our case <code>Technology</code> is used as key and <code>Research</code> as value. We define it based on the list previously defined:</p>
<pre><code>techMap :: Map.Map Technology Research
techMap = Map.fromList $ (\x -&gt; (researchType x, x)) &lt;$&gt; unTechTree techTree</code></pre>
<p>Next time we’ll look into how to actually use all these types and data that were defined.</p>
]]>
</description>
    <itunes:summary><![CDATA[Background
This is rather large topic, so I split it in two episodes. Next one should follow in two weeks if everything goes as planned. First part is about modeling research, while second part concentrates on how things change over time.
There’s three types of research: engineering, natural sciences and social sciences. Research costs points that are produced by various buildings.
Implementation
There’s three database tables, which are defined below:
CurrentResearch
    type Technology
    progress Int
    factionId FactionId

AvailableResearch
    type Technology
    category TopResearchCategory
    factionId FactionId

CompletedResearch
    type Technology
    level Int
    factionId FactionId
    date Int
Data types
Technology is enumeration of all possible technologies. Knowing these enable player to build specific buildings and space ships, enact various laws and so on. In the end this will be (hopefully) large list of technologies.
data Technology =
    HighSensitivitySensors
    | SideChannelSensors
    | HighTensileMaterials
    | SatelliteTechnology
    | BawleyHulls
    | SchoonerHulls
    | CaravelHulls
    ...
    deriving (Show, Read, Eq, Enum, Bounded, Ord)
All research belong to one of the top categories that are shown below:
data TopResearchCategory =
    Eng
    | NatSci
    | SocSci
    deriving (Show, Read, Eq, Ord)
ResearchCategory is more fine grained division of research. Each of the categories is further divided into sub-categories. Only EngineeringSubField is shown below, but other two are similarly divided.
data ResearchCategory =
    Engineering EngineeringSubField
    | NaturalScience NaturalScienceSubField
    | SocialScience SocialScienceSubField
    deriving (Show, Read, Eq)

data EngineeringSubField =
    Industry
    | Materials
    | Propulsion
    | FieldManipulation
    deriving (Show, Read, Eq)
ResearchScore is measure of how big some research is. It has type parameter a that is used to further quantify what kind of ResearchScore we’re talking about.
newtype ResearchScore a = ResearchScore { unResearchScore :: Int }
    deriving (Show, Read, Eq, Ord, Num)
TotalResearchScore is record of three different types of researches. I’m not sure if I should keep it as a record of three fields or if I should change it so that only one of those values can be present at any given time.
data TotalResearchScore a = TotalResearchScore
    { totalResearchScoreEngineering :: ResearchScore EngineeringCost
    , totalResearchScoreNatural :: ResearchScore NaturalScienceCost
    , totalResearchScoreSocial :: ResearchScore SocialScienceCost
    }
    deriving (Show, Read, Eq)
Following singleton values are used with ResearchScore and TotalResearchScore to quantify what kind of value we’re talking about.
data EngineeringCost = EngineeringCost
    deriving (Show, Read, Eq)

data NaturalScienceCost = NaturalScienceCost
    deriving (Show, Read, Eq)

data SocialScienceCost = SocialScienceCost
    deriving (Show, Read, Eq)

data ResearchCost = ResearchCost
    deriving (Show, Read, Eq)

data ResearchProduction = ResearchProduction
    deriving (Show, Read, Eq)

data ResearchLeft = ResearchLeft
    deriving (Show, Read, Eq)
Finally there’s Research, which is a record that uses many of the types introduced earlier. It describes what Technology is unlocked upon completion, what’s the cost is and if there are any technologies that have to have been researched before this research can]]>
</itunes:summary>
    <pubDate>Wed, 22 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2818.ogg" length="20474130" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2818.ogg</guid>
  </item>
  <item>
    <itunes:explicit>no</itunes:explicit>
    <googleplay:explicit>No</googleplay:explicit>
    <title>HPR2817: Are you successful? Click to find out more!</title>
    <author>hackerpublicradio.nospam@nospam.clacke.user.lysator.liu.se (clacke)</author>
    <googleplay:author>hackerpublicradio.nospam@nospam.clacke.user.lysator.liu.se (clacke)</googleplay:author>
    <itunes:author>hackerpublicradio.nospam@nospam.clacke.user.lysator.liu.se (clacke)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2817</link>
    <description><![CDATA[<p>Based on <a href="https://libranet.de/display/0b6b25a8-125c-a71f-c7ae-f1a686792961" class="uri">https://libranet.de/display/0b6b25a8-125c-a71f-c7ae-f1a686792961</a>.</p>
<p>It’s pretty short, less than 4 minutes, but I think it’s important.</p>
<p>Who defines whether you are successful, or whether your project is successful, and does it matter?</p>]]>
</description>
    <itunes:summary><![CDATA[Based on https://libranet.de/display/0b6b25a8-125c-a71f-c7ae-f1a686792961.
It’s pretty short, less than 4 minutes, but I think it’s important.
Who defines whether you are successful, or whether your project is successful, and does it matter?]]>
</itunes:summary>
    <pubDate>Tue, 21 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2817.ogg" length="2867648" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2817.ogg</guid>
  </item>
  <item>
    <itunes:explicit>yes</itunes:explicit>
    <googleplay:explicit>Yes</googleplay:explicit>
    <title>HPR2816: Gnu Awk - Part 14</title>
    <author>perloid.nospam@nospam.autistici.org (Dave Morriss)</author>
    <googleplay:author>perloid.nospam@nospam.autistici.org (Dave Morriss)</googleplay:author>
    <itunes:author>perloid.nospam@nospam.autistici.org (Dave Morriss)</itunes:author>
    <googleplay:image href="http://hackerpublicradio.org/images/hpr_feed_itunes.png"/>
    <link>http://hackerpublicradio.org/eps.php?id=2816</link>
    <description><![CDATA[<h2 id="introduction">Introduction</h2>
<p>This is the fourteenth episode of the “<a href="http://hackerpublicradio.org/series.php?id=94" title="Learning Awk">Learning Awk</a>” series which is being produced by <a href="http://hackerpublicradio.org/correspondents.php?hostid=300" title="b-yeezi">b-yeezi</a> and myself.</p>
<p>In this episode and the next I want to start looking at <em>redirection</em> within Awk programs. I had originally intended to cover the subject in one episode, but there is just too much.</p>
<p>So, in the first episode I will be starting with <a href="https://www.gnu.org/software/gawk/manual/gawk.html#Redirection" title="Redirecting output of `print` and `printf`">output redirection</a> and then in the next episode will spend some time looking at the <code>getline</code> command used for <em>explicit input</em>, often with redirection.</p>
<h2 id="long-notes">Long notes</h2>
<p>I have provided detailed notes as usual for this episode, and these can be <a href="http://hackerpublicradio.org/eps/hpr2816/full_shownotes.html">viewed here</a>.</p>
<h2 id="links">Links</h2>
<ul>
<li><a href="https://www.gnu.org/software/gawk/manual/html_node/index.html"><em>GNU Awk User’s Guide</em></a>
<ul>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Redirection">Redirecting output of <code>print</code> and <code>printf</code></a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Special-FD">Special Files for Standard Preopened Data Streams</a></li>
<li><a href="https://www.gnu.org/software/gawk/manual/gawk.html#Special-Files">Special File names in gawk</a></li>
</ul></li>
</ul>
<ul>
<li>Previous shows in this series on HPR:
<ul>
<li><a href="http://hackerpublicradio.org/eps.php?id=2114">“<em>Gnu Awk - Part 1</em>”</a> - episode 2114</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2129">“<em>Gnu Awk - Part 2</em>”</a> - episode 2129</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2143">“<em>Gnu Awk - Part 3</em>”</a> - episode 2143</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2163">“<em>Gnu Awk - Part 4</em>”</a> - episode 2163</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2184">“<em>Gnu Awk - Part 5</em>”</a> - episode 2184</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2238">“<em>Gnu Awk - Part 6</em>”</a> - episode 2238</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2330">“<em>Gnu Awk - Part 7</em>”</a> - episode 2330</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2438">“<em>Gnu Awk - Part 8</em>”</a> - episode 2438</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2476">“<em>Gnu Awk - Part 9</em>”</a> - episode 2476</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2526">“<em>Gnu Awk - Part 10</em>”</a> - episode 2526</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2554">“<em>Gnu Awk - Part 11</em>”</a> - episode 2554</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2610">“<em>Gnu Awk - Part 12</em>”</a> - episode 2610</li>
<li><a href="http://hackerpublicradio.org/eps.php?id=2804">“<em>Gnu Awk - Part 13</em>”</a> - episode 2804</li>
</ul></li>
<li>Resources:
<ul>
<li><a href="http://hackerpublicradio.org/eps/hpr2816/full_shownotes.epub">ePub version of these notes</a></li>
<li>Examples: <a href="http://hackerpublicradio.org/eps/hpr2816/awk14_fruit_data.txt">awk14_fruit_data.txt</a>, <a href="http://hackerpublicradio.org/eps/hpr2816/awk14_ex1.awk">awk14_ex1.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2816/awk14_ex2.awk">awk14_ex2.awk</a>, <a href="http://hackerpublicradio.org/eps/hpr2816/awk14_ex3.awk">awk14_ex3.awk</a></li>
</ul></li>
</ul>
]]>
</description>
    <itunes:summary><![CDATA[Introduction
This is the fourteenth episode of the “Learning Awk” series which is being produced by b-yeezi and myself.
In this episode and the next I want to start looking at redirection within Awk programs. I had originally intended to cover the subject in one episode, but there is just too much.
So, in the first episode I will be starting with output redirection and then in the next episode will spend some time looking at the getline command used for explicit input, often with redirection.
Long notes
I have provided detailed notes as usual for this episode, and these can be viewed here.
Links

GNU Awk User’s Guide

Redirecting output of print and printf
Special Files for Standard Preopened Data Streams
Special File names in gawk



Previous shows in this series on HPR:

“Gnu Awk - Part 1” - episode 2114
“Gnu Awk - Part 2” - episode 2129
“Gnu Awk - Part 3” - episode 2143
“Gnu Awk - Part 4” - episode 2163
“Gnu Awk - Part 5” - episode 2184
“Gnu Awk - Part 6” - episode 2238
“Gnu Awk - Part 7” - episode 2330
“Gnu Awk - Part 8” - episode 2438
“Gnu Awk - Part 9” - episode 2476
“Gnu Awk - Part 10” - episode 2526
“Gnu Awk - Part 11” - episode 2554
“Gnu Awk - Part 12” - episode 2610
“Gnu Awk - Part 13” - episode 2804

Resources:

ePub version of these notes
Examples: awk14_fruit_data.txt, awk14_ex1.awk, awk14_ex2.awk, awk14_ex3.awk


]]>
</itunes:summary>
    <pubDate>Mon, 20 May 2019 00:00:00 +0000</pubDate>
    <enclosure url="http://hackerpublicradio.org/eps/hpr2816.ogg" length="13233336" type="audio/ogg"/>
    <guid>http://hackerpublicradio.org/eps/hpr2816.ogg</guid>
  </item>
  </channel>
</rss>
