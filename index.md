## The blog of Anthony Hart

### About Me

I'm currently a data science student. I'm interested in dependent type theory, extentional type theories, machine learning, and the foundations of logic an mathematics.

<div class="content list">
  {% for post in site.posts %}
    <div class="list-item">
    <p class="list-post-title">
        <a href="{{ site.baseurl }}{{ post.url }}">- {{ post.title }}</a>
        </p>
    </div>
  {% endfor %}
</div>
