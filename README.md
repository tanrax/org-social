# Org-social

Org-social is a **decentralized social network** that runs on an **Org Mode** file over HTTP.

[Official client](https://github.com/tanrax/org-social.el)

## Quickstart

Create a file called `social.org`.

```sh
M-x find-file RET social.org RET
```

Edit the file and add your basic information:

```org
#+TITLE: Bob's journal
#+NICK: Bob
#+DESCRIPTION: I'm a software developer and I love open source.
#+AVATAR: https://my-awesome-website.com/avatar.jpg
#+LINK: https://my-awesome-website.com
#+FOLLOW_foo: http://foo.org/social.org
#+FOLLOW_jane: https://jane.com/social.org

* Posts
**
:PROPERTIES:
:ID: 2024-12-12T12:00:00+01:00
:END:

This is my first post on Org-social.

**
:PROPERTIES:
:ID: 2025-02-03T23:05:00+01:00
:END:

Welcome to Org-social [[org-social:http://foo.org/social.org][foo]]!

**
:PROPERTIES:
:ID: 2025-02-07T16:00:00+01:00
:REPLY_TO: http://foo.org/social.org/#2025-02-03T23:05:00+01:00
:END:

I forget to ask. Do you need help with Org-social [[org-social:http://foo.org/social.org][foo]]?
```

Now, upload the file to a web server and share the URL with your friends (`https://my-awesome-website.com/social.org`).

Simple.

## Introduction

Org-social is a decentralized social network that leverages the simplicity and power of Org Mode files. It allows users to create, share, and interact with posts in a human-readable format while maintaining compatibility with various text editors and tools. You can publish posts, make replies, mention other users, create polls or personalize your profile. All this without registration, without databases... Just you and your Org Mode file.

It is heavily inspired by [twtxt](https://twtxt.readthedocs.io/en/stable/), [Texudus](https://texudus.com), and the extensions developed by the [Yarn community](https://twtxt.dev/). It takes the best of these specifications, eliminates complex parts, leverages Org Mode's native features, and keeps the premise that social networking should be simple, accessible to both humans and machines, and manageable with standard text editing tools.

The values are:

1. **Simplicity**: Org-social is a simple text file that you can edit with any Emacs or text editor.
2. **Accessibility**: The feed can be read by humans and machines.
3. **Decentralization**: You are a node in the network. The feed is hosted on a web server and can be accessed by anyone.
4. **Org Philosophy**: Native Org Mode features are used to enhance the social experience, such as timestamps, properties, and links.

## Tutorial

### 1. Create an Org Mode file called `social.org`

```sh
M-x find-file RET social.org RET
```

### 2. Edit the file and add your basic information

```org
#+TITLE: My journal
#+NICK: YourNick
#+DESCRIPTION: A brief description about yourself
```

### 3. Add your first post

The format uses Org Mode headlines with timestamps. Each new post must be added under the `* Posts` section.

```org
* Posts
```

Now you can add your first post.

```org
**
:PROPERTIES:
:ID: 2025-04-28T12:00:00+01:00
:END:

This is my first post on Org-social.
```

The header `**` indicates a new post. The `:PROPERTIES:` drawer is used to add metadata to the post, such as the unique identifier (`ID`) and other optional properties.

The datetime in the `ID` property is the unique identifier of each post. It must be in a subset of the *RFC 3339 format*. matching any of the forms:

* `####-##-##T##:##:##+##:##` e.g. `2025-12-30T20:30:15+00:00`, `2025-12-30T22:30:15+02:00`
* `####-##-##T##:##:##-##:##` (not including an offset of `00:00`) e.g. `2025-12-30T18:30:15-02:00`

The result will be:

```org
#+TITLE: My journal
#+NICK: YourNick
#+DESCRIPTION: A brief description about yourself

* Posts
**
:PROPERTIES:
:ID: 2025-04-28T12:00:00+01:00
:END:

This is my first post on Org-social.
```

The file must be encoded with UTF-8 and should use LF (`\n`) as line separators.

### 4. Upload the file to a web server

You can use any web server, but make sure it supports plain text files. You can use GitHub Pages, Gitea, or any other service that allows you to host plain text files.

To consume and read other users' feeds, you can read them manually, use a script, or a client.

## Syntax

### Comments

Every line that starts with a hash sign (`#`) outside of Org Mode syntax is considered a comment. However, prefer using Org Mode's native comment syntax:

```org
# This is an Org Mode comment
# This one too

#+BEGIN_COMMENT
This is a comment block
that can span multiple lines
#+END_COMMENT
```

### Global Metadata

Global metadata is defined using Org Mode's standard keywords at the top of the file:

```org
#+TITLE: My Awesome journal
#+NICK: Bob
#+DESCRIPTION: I love Emacs.
#+AVATAR: https://example.com/avatar.jpg
#+LINK: https://my-awesome-website.com
#+LINK: https://my-blog.com
#+FOLLOW: jane https://jane.com/social.org
#+FOLLOW: lucy https://lucy.com/social.org
#+CONTACT: mailto:my-email@example.com
#+CONTACT: xmpp:my@account.com
#+CONTACT: https://mastodon.social/@my-account
```

| Field | Description | Multiple |
|-------|-------------|----------|
| `TITLE` | The title of your social feed | No |
| `NICK` | Your nickname. This is the name that will be displayed in posts. You cannot use spaces. | No |
| `DESCRIPTION` | A short description about yourself | No |
| `AVATAR` | The URL of your avatar image | No |
| `LINK` | Links to your personal website or profile | Yes |
| `FOLLOW` | Users you follow. Format: `nickname https://example.com/social.org` | Yes |
| `CONTACT` | Contact information: Email, XMPP, Matrix, ActivityPub, etc. | Yes |

### Post Metadata

Each post uses Org Mode's properties drawer for metadata:

```org
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:LANG: en
:TAGS: programming social
:CONTENT_WARNING: yes
:CLIENT: org-social.el
:REPLY_TO: http://foo.org/social.org/#2025-02-03T23:05:00+01:00
:MOOD: 😊
:END:

This is the content of my post with some metadata.
```

Available properties:

| Property | Description |
|----------|-------------|
| `ID` | Unique timestamp identifier (required) |
| `LANG` | Language code of the post |
| `TAGS` | Space-separated tags |
| `CONTENT_WARNING` | Content warning label |
| `CLIENT` | Client application used |
| `REPLY_TO` | ID of post being replied to |
| `REPLY_URL` | URL of the feed being replied to |
| `POLL_END` | End time for polls (RFC 3339 format) |
| `POLL_OPTION` | Selected option in a poll vote |
| `MOOD` | Mood indicator |

### Mentions

Org-social uses Org Mode's link system for mentions. First, you can define a custom link type:

```org
[[org-social:http://example.org/social.org][username]]
```

Then mention users using this format:

```org
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

Welcome to Org-social [[org-social:http://example.org/social.org][bob]]!
```

You can mention multiple users in a single post:

```org
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

Good morning [[org-social:http://example.org/social.org][bob]] and [[org-social:http://alice.com/social.org][alice]]! What are you doing today?
```

### Multiline Posts

Org Mode naturally supports multiline content. Unlike plain text formats, you can write posts with multiple paragraphs, lists, and rich formatting:

```org
** <2025-05-01 Thu 12:00> A rich post
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

This is a multiline post with rich content.

I can include:
- Lists with multiple items
- *Bold text* and /italic text/
- Code snippets: ~print("hello")~
- Links: [[https://example.com][Example website]]

#+BEGIN_SRC python
def parse_org_social(file_path):
	"""Parse an Org-social file"""
	with open(file_path, 'r') as f:
		content = f.read()
	# Implementation here
	return content
#+END_SRC

And much more!
```

### Polls

Polls use Org Mode's checkbox lists with special properties:

```org
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:POLL_END: 2025-05-01T13:00:00+01:00
:END:

What's your favorite programming language?

Choose your favorite:

- [ ] Python
- [ ] JavaScript
- [ ] PHP
- [ ] Emacs Lisp
```

To vote on a poll, create a vote post:

```org
**
:PROPERTIES:
:ID: 2025-05-01T13:00:00+01:00
:REPLY_TO: http://example.org/social.org/#2025-05-01T12:00:00+01:00
:POLL_OPTION: Emacs Lisp
:END:

I choose Emacs Lisp as my favorite programming language!
```

### Media Attachments

Reference media files using Org Mode's link syntax. You can link to images, videos, or any other media:

```org
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

I want to share [[https://www.gnu.org/software/emacs/manual/pdf/emacs.pdf][Emacs Manual PDF]] with everyone.
```

## Discoverability

Because of the decentralised nature it is very difficult to discover new users. You have to think of it as a technology similar to email or RSS feeds. The natural flow to find new addresses, URLs, or nodes, is because you have been given the address or because you have seen a link on a website. Org-social is the same. You have to share your address with your friends or on social media.

To take your first steps you can start interacting with a list of people who have been adding their feed to an Org-social feeds registry. You can also use community channels to find new users.

## FAQ

### Can I use it as a simple substitute for RSS/Atom?

Yes, check this example:

```org
#+TITLE: Bob's Blog Feed
#+NICK: Bob_feed
#+DESCRIPTION: This is my blog feed.
#+AVATAR: https://blog.example.com/avatar.jpg

* Posts
**
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:TITLE: My awesome title
:LANG: en
:CATEGORY: programming
:URL: https://blog.example.com/my-awesome-article
:END:

This is my awesome article content!
```

### Is there pagination?

Yes. You can use the HTTP header `Content-Range` with `Content-Length` to paginate the feed. Check the [HTTP Range Requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests) documentation.

### What is the best way to host my feed?

You can use any web server that supports plain text files. GitHub Pages, Gitea, or any other service that allows you to host plain text files. You can also use a self-hosted solution like [Nginx](https://www.nginx.com/) or [Apache](https://httpd.apache.org/).

### How often do I need to check the followers' feeds?

It depends on how you use it. Check the header `Last-Modified` to see if the feed has changed. If the feed has changed, you can fetch the new feed.

### Can I modify a post after I publish it or delete it?

Yes, you can modify or delete a post after you publish it. Just edit the file and upload it again keeping the original timestamp unchanged (otherwise you are making a new post and will break replies to your post). Clients will fetch the new file and update your profile.

### Is there a official client?

Yes, [org-social.el](https://github.com/tanrax/org-social.el).

### Do you have a feed to share?

Make a Pull Request by adding your URL to `registers.txt`.

## Changelogs

## 1.1

- Removed the separation between `* Posts` and the first header 2 `**`.

Before

```org
* Posts

**
```

After

```org
* Posts
**
```

Thanks [@confusedalex](https://github.com/confusedalex).

## 1.0

First draft
