# Org-social

Org-social is a **decentralized social network** that runs on an **Org Mode** file over HTTP.

For example, you can create a file called `social.org` with the following content:

```org
#+TITLE: Bob's Social Network
#+AUTHOR: Bob
#+DESCRIPTION: I'm a software developer and I love open source.
#+AVATAR: https://my-awesome-website.com/avatar.jpg
#+WEBSITE: https://my-awesome-website.com
#+FOLLOW: foo http://foo.org/org-social.org
#+FOLLOW: jane https://jane.com/org-social.org

* Posts

** <2024-12-12 Thu 12:00> Hello world!
:PROPERTIES:
:ID: 2024-12-12T12:00:00+01:00
:END:

This is my first post on Org-social.

** <2025-02-03 Mon 23:05> Welcome
:PROPERTIES:
:ID: 2025-02-03T23:05:00+01:00
:END:

Welcome to Org-social [[org-social:foo@http://foo.org/org-social.org][foo]]!

** <2025-02-07 Fri 16:00> Follow-up question
:PROPERTIES:
:ID: 2025-02-07T16:00:00+01:00
:REPLY_TO: 2025-02-03T23:05:00+01:00
:END:

I forget to ask. Do you need help with Org-social [[org-social:foo@http://foo.org/org-social.org][foo]]?

** <2025-02-08 Sat 12:00> Agreement
:PROPERTIES:
:ID: 2025-02-08T12:00:00+01:00
:REPLY_TO: 2025-01-21T05:05:00+01:00
:REPLY_URL: http://boo.com/org-social.org
:END:

I agree with you!
```

Then you can upload the file to a web server and share the URL with your friends.

You can publish posts, make replies, mention other users, create polls or personalize your profile. All this without registration, without databases... Just you and your Org Mode file.

Org-social is heavily inspired by [twtxt](https://twtxt.readthedocs.io/en/stable/), [Texudus](https://texudus.com), and the extensions developed by the [Yarn community](https://twtxt.dev/). It takes the best of these specifications, eliminates complex parts, leverages Org Mode's native features, and keeps the premise that social networking should be simple, accessible to both humans and machines, and manageable with standard text editing tools.

The values are:

1. **Simplicity**: Org-social is a simple text file that you can edit with any Emacs or text editor.
2. **Accessibility**: The feed can be read by humans and machines.
3. **Decentralization**: You are a node in the network. The feed is hosted on a web server and can be accessed by anyone.
4. **Org Philosophy**: Native Org Mode features are used to enhance the social experience, such as timestamps, properties, and links.

## Quickstart

### 1. Create an Org Mode file called `social.org`

```sh
M-x find-file RET social.org RET
```

### 2. Edit the file and add your basic information

```org
#+TITLE: My Social Network
#+AUTHOR: YourNick
#+DESCRIPTION: A brief description about yourself
```

### 3. Add your first post

The format uses Org Mode headlines with timestamps. Each new post must be added under the `* Posts` section.

```org
* Posts
```

Now you can add your first post. You can use the Org Mode timestamp feature to insert the current date and time:

```org
** <2025-04-28 Mon 12:00> Hello world!
:PROPERTIES:
:ID: 2025-04-28T12:00:00+01:00
:END:

This is my first post on Org-social.
```

You can insert the timestamp manually, or use Emacs' built-in command to insert a timestamp:

```emacs-lisp
M-x org-time-stamp
```

The result will be:

```org
#+TITLE: My Social Network
#+AUTHOR: YourNick
#+DESCRIPTION: A brief description about yourself

* Posts

** <2025-04-28 Mon 12:00> Hello world!
:PROPERTIES:
:ID: 2025-04-28T12:00:00+01:00
:END:

This is my first post on Org-social.
```

The datetime in the `ID` property is the unique identifier of each post. It must be in *ISO 8601 format*. The time zone, `+01:00`, isn't optional.

The file must be encoded with UTF-8 and should use LF (`\n`) as line separators.

### 4. Upload the file to a web server

You can use any web server, but make sure it supports plain text files. You can use GitHub Pages, Gitea, or any other service that allows you to host plain text files.

To consume and read other users' feeds, you can read them manually, use a script, or a client.

## Comments

Every line that starts with a hash sign (`#`) outside of Org Mode syntax is considered a comment. However, prefer using Org Mode's native comment syntax:

```org
# This is an Org Mode comment
# This one too

#+BEGIN_COMMENT
This is a comment block
that can span multiple lines
#+END_COMMENT
```

## Global Metadata

Global metadata is defined using Org Mode's standard keywords at the top of the file:

```org
#+TITLE: My Social Network
#+AUTHOR: Bob
#+DESCRIPTION: I'm a software developer and I love open source.
#+AVATAR: https://example.com/avatar.jpg
#+WEBSITE: https://my-awesome-website.com
#+FOLLOW: jane https://jane.com/org-social.org
#+FOLLOW: lucy https://lucy.com/org-social.org
#+CONTACT: mailto:my-email@example.com
#+CONTACT: xmpp:my@account.com
#+CONTACT: https://mastodon.social/@my-account
#+PUBLIC_KEY: 1234567890abcdef
#+DM_URL: https://example.com/dm.org
```

| Field | Description | Multiple |
|-------|-------------|----------|
| `TITLE` | The title of your social feed | No |
| `AUTHOR` | Your nickname. This is the name that will be displayed in posts. You cannot use spaces. | No |
| `DESCRIPTION` | A short description about yourself | No |
| `AVATAR` | The URL of your avatar image | No |
| `WEBSITE` | Your personal website URL | No |
| `FOLLOW` | Users you follow. Format: `nickname https://url/org-social.org` | Yes |
| `CONTACT` | Contact information: Email, XMPP, Matrix, ActivityPub, etc. | Yes |
| `PUBLIC_KEY` | Your public key for encryption and signing | No |
| `DM_URL` | The URL for direct messages file | No |

## Post Structure

Posts are organized under headlines using Org Mode's hierarchical structure:

```org
* Posts                           # Main section for posts
* Polls                          # Polls section
* Media                          # Media files section
```

## Post Metadata

Each post uses Org Mode's properties drawer for metadata:

```org
** <2025-05-01 Thu 12:00> My awesome post
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:LANG: en
:TAGS: programming social
:CONTENT_WARNING: spoiler
:CLIENT: org-social.el
:VERSION: 1.0
:REPLY_TO: 2025-04-30T15:30:00+01:00
:REPLY_URL: https://another-user.com/org-social.org
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
| `VERSION` | Client version |
| `REPLY_TO` | ID of post being replied to |
| `REPLY_URL` | URL of the feed being replied to |
| `LOCATION` | Geographic location |
| `MOOD` | Mood indicator |

## Mentions

Org-social uses Org Mode's link system for mentions. First, you can define a custom link type:

```org
#+LINK: org-social https://
```

Then mention users using this format:

```org
** <2025-05-01 Thu 12:00> Welcome message
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

Welcome to Org-social [[org-social:bob@http://example.org/org-social.org][bob]]!
```

You can mention multiple users in a single post:

```org
** <2025-05-01 Thu 12:00> Good morning
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

Good morning [[org-social:bob@http://example.org/org-social.org][bob]] and [[org-social:alice@http://alice.com/org-social.org][alice]]! What are you doing today?
```

## Replies and Threads

Replies use the `REPLY_TO` property to reference the original post's ID:

```org
** <2025-05-01 Thu 12:00> My reply
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:REPLY_TO: 2025-04-28T12:00:00+01:00
:REPLY_URL: http://example.org/org-social.org
:END:

I agree with your previous post!
```

For replies to your own posts, you can omit the `REPLY_URL`:

```org
** <2025-05-01 Thu 12:00> Self reply
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:REPLY_TO: 2025-04-28T12:00:00+01:00
:END:

Adding more thoughts to my previous post.
```

You can also use Org Mode's hierarchical structure for threaded conversations:

```org
** <2025-05-01 Thu 12:00> Original post
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

What do you think about Org-social?

*** <2025-05-01 Thu 13:00> First reply
:PROPERTIES:
:ID: 2025-05-01T13:00:00+01:00
:REPLY_TO: 2025-05-01T12:00:00+01:00
:END:

I think it's great!

**** <2025-05-01 Thu 14:00> Sub-reply
:PROPERTIES:
:ID: 2025-05-01T14:00:00+01:00
:REPLY_TO: 2025-05-01T13:00:00+01:00
:END:

I agree completely!
```

## Multiline Posts

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

And much more!
```

## Polls

Polls use Org Mode's checkbox lists with special properties:

```org
* Polls

** <2025-05-01 Thu 12:00> What's your favorite programming language?
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:POLL_END: <2025-05-03 Sat 12:00>
:END:

Choose your favorite:

- [ ] Python
- [ ] JavaScript
- [ ] PHP
- [ ] Emacs Lisp
```

To vote on a poll, create a vote post:

```org
** <2025-05-01 Thu 13:00> My vote
:PROPERTIES:
:ID: 2025-05-01T13:00:00+01:00
:POLL_VOTE: 2025-05-01T12:00:00+01:00
:POLL_OPTION: 1
:POLL_URL: http://example.org/org-social.org
:END:

I choose Python!
```

The `POLL_OPTION` is the index starting from 1. You can omit `POLL_URL` when voting on your own polls.

Polls can have end dates using Org Mode timestamps in the `POLL_END` property.

## Source Code Sharing

Use Org Mode's source blocks for sharing code:

```org
** <2025-05-01 Thu 14:00> Sharing some code
:PROPERTIES:
:ID: 2025-05-01T14:00:00+01:00
:TAGS: programming python
:END:

Here's a function to parse Org-social feeds:

#+BEGIN_SRC python
def parse_org_social(file_path):
	"""Parse an Org-social file"""
	with open(file_path, 'r') as f:
		content = f.read()
	# Implementation here
	return content
#+END_SRC
```

## Tables and Structured Data

Org Mode's table support can be used for structured information:

```org
* Following List

| User | URL | Last Updated |
|------+-----+--------------|
| alice | https://alice.com/org-social.org | <2025-05-01 Thu> |
| bob | https://bob.dev/org-social.org | <2025-04-30 Wed> |
```

## Media Attachments

Reference media files using Org Mode's attachment system or simple links:

```org
** <2025-05-01 Thu 12:00> My trip to Spain
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

I traveled to Spain! Check out this photo:

[[https://example.com/static/me-in-spain.jpg]]

Or using Org Mode attachments:

[[attachment:spain-photo.jpg]]
```

## Follow Management

Following other users is managed through the global metadata:

```org
#+FOLLOW: alice https://alice.com/org-social.org
#+FOLLOW: bob https://bob.dev/org-social.org
#+FOLLOW: charlie https://charlie.org/org-social.org
```

Or using a structured approach:

```org
* Following

** Alice
:PROPERTIES:
:URL: https://alice.com/org-social.org
:LAST_CHECKED: <2025-05-01 Thu 10:00>
:END:

Great content about AI research.

** Bob
:PROPERTIES:
:URL: https://bob.dev/org-social.org
:LAST_CHECKED: <2025-05-01 Thu 09:30>
:END:

Excellent programming tutorials.
```

## Discoverability

Because of the decentralised nature it is very difficult to discover new users. You have to think of it as a technology similar to email or RSS feeds. The natural flow to find new addresses, URLs, or nodes, is because you have been given the address or because you have seen a link on a website. Org-social is the same. You have to share your address with your friends or on social media.

To take your first steps you can start interacting with a list of people who have been adding their feed to an Org-social feeds registry. You can also use community channels to find new users.

## FAQ

### Can I use it as a simple substitute for RSS/Atom?

Yes, check this example:

```org
#+TITLE: Bob's Blog Feed
#+AUTHOR: Bob_feed
#+DESCRIPTION: This is my blog feed.
#+AVATAR: https://blog.example.com/avatar.jpg

* Posts

** <2025-05-01 Thu 12:00> My awesome article
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:TITLE: My awesome title
:LANG: en
:CATEGORY: programming
:URL: https://blog.example.com/my-awesome-article
:END:

This is my awesome article content!
```

### Can I use rich formatting?

Yes! Org Mode supports rich formatting natively:
- *Bold*, /italic/, _underlined_, =code=, ~verbatim~
- Lists (bulleted, numbered, checklists)
- Tables
- Links
- Images
- Source code blocks
- Mathematical expressions
- And much more

### Is there pagination?

Yes. You can use the HTTP header `Content-Range` with `Content-Length` to paginate the feed. Check the [HTTP Range Requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Range_requests) documentation.

### What is the best way to host my feed?

You can use any web server that supports plain text files. GitHub Pages, Gitea, or any other service that allows you to host plain text files. You can also use a self-hosted solution like [Nginx](https://www.nginx.com/) or [Apache](https://httpd.apache.org/).

### Can I use hashtags?

Yes! You can use hashtags in your posts or use Org Mode's native tagging system:

```org
** <2025-05-01 Thu 12:00> Hello Org-social   :orgsocial:social:
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:TAGS: orgsocial social networking
:END:

Hi Org-social! #orgsocial #socialnetwork
```

### How often do I need to check the followers' feeds?

It depends on how you use it. Check the header `Last-Modified` to see if the feed has changed. If the feed has changed, you can fetch the new feed.

### Can I modify a post after I publish it or delete it?

Yes, you can modify or delete a post after you publish it. Just edit the file and upload it again keeping the original timestamp unchanged (otherwise you are making a new post and will break replies to your post). Clients will fetch the new file and update your profile.

### Is there a official client?

No, at the moment there is no official client.

## Community

Join the community to discuss Org-social development and share your feeds:

- IRC: #org-social on irc.libera.chat
