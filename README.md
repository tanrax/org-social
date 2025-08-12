# Org-Social

Org-Social is a **decentralized social network** that runs on an **Org Mode** file over HTTP.

For example, you can create a file called `org-social.org` with the following content:

```org
#+TITLE: Bob's Social Network
#+AUTHOR: Bob
#+EMAIL: bob@my-awesome-website.com
#+DESCRIPTION: I'm a software developer and I love open source.
#+AVATAR: https://my-awesome-website.com/avatar.jpg
#+WEBSITE: https://my-awesome-website.com
#+FOLLOW: foo http://foo.org/org-social.org
#+FOLLOW: jane https://jane.com/org-social.org
#+PUBLIC_KEY: 1234567890abcdef
#+DM_URL: https://my-awesome-website.com/dm.org

* Posts

** <2024-12-12 Thu 12:00> Hello world!
:PROPERTIES:
:ID: 2024-12-12T12:00:00+01:00
:END:

This is my first post on Org-Social.

** <2025-02-03 Mon 23:05> Welcome
:PROPERTIES:
:ID: 2025-02-03T23:05:00+01:00
:END:

Welcome to Org-Social [[org-social:foo@http://foo.org/org-social.org][foo]]!

** <2025-02-07 Fri 16:00> Follow-up question
:PROPERTIES:
:ID: 2025-02-07T16:00:00+01:00
:REPLY_TO: 2025-02-03T23:05:00+01:00
:END:

I forget to ask. Do you need help with Org-Social [[org-social:foo@http://foo.org/org-social.org][foo]]?

** <2025-02-08 Sat 12:00> Agreement
:PROPERTIES:
:ID: 2025-02-08T12:00:00+01:00
:REPLY_TO: 2025-01-21T05:05:00+01:00
:REPLY_URL: http://boo.com/org-social.org
:END:

I agree with you!
```

Then you can upload the file to a web server and share the URL with your friends.

You can publish posts, make replies, mention other users, send direct messages, create polls or personalize your profile. All this without registration, without databases... Just you and your Org Mode file.

Org-Social is heavily inspired by [twtxt](https://twtxt.readthedocs.io/en/stable/), [Texudus](https://texudus.com), and the extensions developed by the [Yarn community](https://twtxt.dev/). It takes the best of these specifications, eliminates complex parts, leverages Org Mode's native features, and keeps the premise that social networking should be simple, accessible to both humans and machines, and manageable with standard text editing tools.

The values are:

1. **Simplicity**: The specification is simple and easy to understand.
2. **Accessibility**: The feed can be read by humans and machines.
3. **Decentralization**: You are a node in the network. The feed is hosted on a web server and can be accessed by anyone.
4. **Org Philosophy**: The feed is an Org Mode file that can be managed with Emacs and any text editor. You can use `curl`, `wget`, `cat`, `grep`, `sed`, and any other tool you like to manage your feed.
5. **Rich Structure**: Leverages Org Mode's hierarchical structure and metadata capabilities.

## Quickstart

### 1. Create an Org Mode file called `org-social.org`

```sh
$ touch ~/org-social.org
```

### 2. Edit the file and add your basic information

```org
#+TITLE: My Social Network
#+AUTHOR: YourNick
#+EMAIL: your@email.com
#+DESCRIPTION: A brief description about yourself
```

### 3. Add your first post

The format uses Org Mode headlines with timestamps. Each new post must be added under the `* Posts` section.

You can use Emacs, `vim`, `vscode`, or any text editor. For example, using Emacs:

```emacs-lisp
M-x org-time-stamp
```

Or with a simple unix command:

```sh
echo "** $(date '+<%Y-%m-%d %a %H:%M>') Hello world!" >> org-social.org
echo ":PROPERTIES:" >> org-social.org
echo ":ID: $(date -Im)" >> org-social.org
echo ":END:" >> org-social.org
echo "" >> org-social.org
echo "This is my first post on Org-Social." >> org-social.org
```

The result will be:

```org
#+AUTHOR: YourNick

* Posts

** <2025-04-28 Mon 12:00> Hello world!
:PROPERTIES:
:ID: 2025-04-28T12:00:00+01:00
:END:

This is my first post on Org-Social.
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
#+EMAIL: bob@example.com
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
| `EMAIL` | Your email address | No |
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
** <timestamp> Post title        # Individual post
*** Reply                        # Reply to a post
**** Sub-reply                   # Nested reply

* Polls                          # Polls section
* Direct Messages                # Direct messages section
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
:LOCATION: Madrid, Spain
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

Org-Social uses Org Mode's link system for mentions. First, you can define a custom link type:

```org
#+LINK: org-social https://
```

Then mention users using this format:

```org
** <2025-05-01 Thu 12:00> Welcome message
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:END:

Welcome to Org-Social [[org-social:bob@http://example.org/org-social.org][bob]]!
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

What do you think about Org-Social?

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

Here's a function to parse Org-Social feeds:

#+BEGIN_SRC python
def parse_org_social(file_path):
	"""Parse an Org-Social file"""
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

## Direct Messages

Direct messages can be stored in a separate section or file with encryption support:

```org
* Direct Messages
:PROPERTIES:
:ENCRYPTED: t
:END:

** <2025-05-02 Fri 10:00> Private message
:PROPERTIES:
:ID: 2025-05-02T10:00:00+01:00
:TO: alice@https://alice.com/org-social.org
:ENCRYPTED_CONTENT: [encrypted content here]
:END:
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

## Agenda Integration

Posts can be integrated with Org Mode's agenda system:

```org
** TODO <2025-05-02 Fri> Reply to Alice's message
SCHEDULED: <2025-05-02 Fri>
:PROPERTIES:
:ID: task-001
:END:

Remember to reply to Alice's message about the project.

** DONE <2025-05-01 Thu> Published post about Emacs
CLOSED: [2025-05-01 Thu 12:34]
:PROPERTIES:
:ID: task-002
:END:
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

Because of the decentralised nature it is very difficult to discover new users. You have to think of it as a technology similar to email or RSS feeds. The natural flow to find new addresses, URLs, or nodes, is because you have been given the address or because you have seen a link on a website. Org-Social is the same. You have to share your address with your friends or on social media.

To take your first steps you can start interacting with a list of people who have been adding their feed to an Org-Social feeds registry. You can also use community channels to find new users.

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
** <2025-05-01 Thu 12:00> Hello Org-Social   :orgsocial:social:
:PROPERTIES:
:ID: 2025-05-01T12:00:00+01:00
:TAGS: orgsocial social networking
:END:

Hi Org-Social! #orgsocial #socialnetwork
```

### How often do I need to check the followers' feeds?

It depends on how you use it. Check the header `Last-Modified` to see if the feed has changed. If the feed has changed, you can fetch the new feed.

### Can I modify a post after I publish it or delete it?

Yes, you can modify or delete a post after you publish it. Just edit the file and upload it again keeping the original timestamp unchanged (otherwise you are making a new post and will break replies to your post). Clients will fetch the new file and update your profile.

### Can I make an Org-Social client in my favorite programming language?

Absolutely! Org Mode files are just text files with a well-defined structure. You can parse them using existing Org Mode parsers available in many languages, or write your own parser.

## Emacs Lisp Functions

Here are some useful Emacs Lisp functions for working with Org-Social:

```emacs-lisp
(defun org-social-new-post (content)
  "Create a new Org-Social post"
  (interactive "sPost content: ")
  (let ((timestamp (format-time-string "<%Y-%m-%d %a %H:%M>")))
    (goto-char (point-max))
    (insert "\n** " timestamp " " content "\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: " (format-time-string "%Y-%m-%dT%H:%M:%S%z") "\n")
    (insert ":END:\n\n")))

(defun org-social-mention-user (nick url)
  "Insert a mention to another user"
  (interactive "sUser nick: \nsUser URL: ")
  (insert (format "[[org-social:%s@%s][%s]]" nick url nick)))

(defun org-social-reply-to-post (post-id content)
  "Create a reply to an existing post"
  (interactive "sPost ID to reply to: \nsReply content: ")
  (org-social-new-post content)
  (org-set-property "REPLY_TO" post-id))

(defun org-social-create-poll (question options end-date)
  "Create a new poll"
  (interactive "sQuestion: \nsOptions (comma-separated): \nsEnd date: ")
  (let ((timestamp (format-time-string "<%Y-%m-%d %a %H:%M>")))
    (goto-char (point-max))
    (insert "\n** " timestamp " " question "\n")
    (insert ":PROPERTIES:\n")
    (insert ":ID: " (format-time-string "%Y-%m-%dT%H:%M:%S%z") "\n")
    (when end-date
      (insert ":POLL_END: " end-date "\n"))
    (insert ":END:\n\n")
    (dolist (option (split-string options ","))
      (insert "- [ ] " (string-trim option) "\n"))))
```

## Community

Join the community to discuss Org-Social development and share your feeds:

- IRC: #org-social on irc.libera.chat
- Matrix: #org-social:matrix.org
- Mailing list: org-social@lists.sr.ht

## Icon

You can create your own icon for Org-Social or use the Org Mode logo as inspiration, respecting its licensing terms.
