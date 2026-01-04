# Changelog

## 1.6

- Added `#+LOCATION:` global metadata field for user location (city, country).
- Added `#+BIRTHDAY:` global metadata field for user birthday in YYYY-MM-DD format.
- Added `#+LANGUAGE:` global metadata field for space-separated language codes (ISO 639-1) that the user speaks.
- Added `#+PINNED:` global metadata field to pin a post to the top of the profile using its ID (timestamp).
- Post ID can now be specified in the header (after `**`) in addition to the `:ID:` property in the properties drawer. Both formats are valid. If both are present, the header value takes priority.

## 1.5

- Added `:VISIBILITY:` property to control post visibility. When set to `mention`, posts should only be displayed to mentioned users extracted from org-social links in the post body.

## 1.4

- Added `:INCLUDE:` property to boost/share posts from other users.
- Added `:MIGRATION:` property to report domain changes.


## 1.3

- Added `:MOOD:` property to express reactions to posts.
- Added `:GROUP:` property to subscribe to groups and publish posts in groups.

## 1.2

- Avatar image must be at least 128x128 pixels in JPG or PNG format.

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

- Removed `:REPLY_URL:` property.

Thanks [@confusedalex](https://github.com/confusedalex).

## 1.0

First draft
