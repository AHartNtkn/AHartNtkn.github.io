### About Me

I'm currently a data science student. I'm interested in dependent type theory, extensional type theories, machine learning, and the foundations of logic and mathematics.

## Blog Posts

<div class="content list">
  {% for post in site.posts %}
    <div class="list-item">
    <p class="list-post-title">
        <a href="{{ site.baseurl }}{{ post.url }}">- {{ post.title }}</a> (<small>{{post.date | date: "%m/%d/%y" }}</small>)
        </p>
    </div>
  {% endfor %}
</div>
