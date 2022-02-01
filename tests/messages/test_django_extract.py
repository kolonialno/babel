# -- encoding: UTF-8 --
from io import BytesIO
import pytest
from babel.messages import extract

def test_django_translate():
    buf = BytesIO(br"""
{% translate "foo" %}
""")
    messages = list(extract.extract('django', buf,
                                    extract.DEFAULT_KEYWORDS, [], {}))
    assert messages[0][1] == u'foo'
    assert messages[0][4] == set()

def test_django_translate():
    """Django doesn't support variables in {% translate %}"""
    buf = BytesIO(br"""
{% translate "foo {bar}" %}
""")
    messages = list(extract.extract('django', buf,
                                    extract.DEFAULT_KEYWORDS, [], {}))
    assert messages[0][1] == u'foo {bar}'
    assert messages[0][4] == set()

def test_django_blocktranslate():
    buf = BytesIO(br"""
{% blocktranslate %}
foo
{% endblocktranslate %}
""")
    messages = list(extract.extract('django', buf,
                                    extract.DEFAULT_KEYWORDS, [], {}))
    assert messages[0][1] == u'\nfoo\n'
    assert messages[0][4] == set()

def test_django_blocktranslate_format():
    buf = BytesIO(br"""
{% blocktranslate trimmed %}
foo {bar}
{% endblocktranslate %}
""")
    messages = list(extract.extract('django', buf,
                                    extract.DEFAULT_KEYWORDS, [], {}))
    assert messages[0][1] == u'foo %(bar)s'
    assert messages[0][4] == {u'python-format'}
