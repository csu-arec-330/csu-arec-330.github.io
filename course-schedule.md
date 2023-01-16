---
layout: page
title: Course Schedule
permalink: /course-schedule/
description: Listing of course modules, topics, dates, and assignments
---


{% for module in site.modules %}
{{ module }}
{% endfor %}