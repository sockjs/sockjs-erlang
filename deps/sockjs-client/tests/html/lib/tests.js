var arrIndexOf, batch_factory_factory, batch_factory_factory_amp, chunking_test_factory, echo_factory_factory, factor_batch_large, factor_batch_large_amp, factor_echo_basic, factor_echo_large_message, factor_echo_rich, factor_echo_special_chars, factor_echo_unicode, factor_server_close, factor_user_close, newSockJS, protocol, protocols, test_invalid_url_404, test_invalid_url_500, test_invalid_url_port, test_protocol_errors, test_protocol_messages, _i, _j, _len, _len2;
protocols = ['websocket', 'xhr-streaming', 'iframe-eventsource', 'iframe-htmlfile', 'xhr-polling', 'iframe-xhr-polling', 'jsonp-polling'];
newSockJS = function(path, protocol) {
  var url;
  url = /^http/.test(path) ? path : client_opts.url + path;
  return new SockJS(url, [protocol], client_opts.sockjs_opts);
};
echo_factory_factory = function(protocol, messages) {
  return function() {
    var a, r;
    expect(2 + messages.length);
    a = messages.slice(0);
    r = newSockJS('/echo', protocol);
    r.onopen = function(e) {
      log('onopen ' + e);
      ok(true);
      return r.send(a[0]);
    };
    r.onmessage = function(e) {
      deepEqual(e.data, a[0]);
      a.shift();
      if (typeof a[0] === 'undefined') {
        return r.close();
      } else {
        return r.send(a[0]);
      }
    };
    return r.onclose = function(e) {
      if (a.length) {
        ok(false, "Transport closed prematurely. " + e);
      } else {
        ok(true);
      }
      return start();
    };
  };
};
factor_echo_basic = function(protocol) {
  var messages;
  messages = ['data'];
  return echo_factory_factory(protocol, messages);
};
factor_echo_rich = function(protocol) {
  var messages;
  messages = [
    [1, 2, 3, 'data'], null, "data", 1, 12.0, {
      a: 1,
      b: 2
    }
  ];
  return echo_factory_factory(protocol, messages);
};
factor_echo_unicode = function(protocol) {
  var messages;
  messages = ["Τη γλώσσα μου έδωσαν ελληνική το σπίτι φτωχικό στις αμμουδιές του ", "ღმერთსი შემვედრე, ნუთუ კვლა დამხსნას სოფლისა შრომასა, ცეცხლს, წყალს", "⠊⠀⠉⠁⠝⠀⠑⠁⠞⠀⠛⠇⠁⠎⠎⠀⠁⠝⠙⠀⠊⠞⠀⠙⠕⠑⠎⠝⠞⠀⠓⠥⠗⠞⠀⠍⠑", "Би шил идэй чадна, надад хортой биш", "을", "나는 유리를 먹을 수 있어요. 그래도 아프지 않아요", "ฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บฉันกินกระจกได้ แต่มันไม่ทำให้ฉันเจ็บ", "Ég get etið gler án þess að meiða mig.", "Mogę jeść szkło, i mi nie szkodzi.", "\ufffd\u10102\u2f877", "Начало музыкальной карьеры\nБритни пела в церковном хоре местной баптистской церкви. В возрасте 8-ми лет Спирс прошла аудирование для участия в шоу «Новый Клуб Микки-Мауса» на канале «Дисней». И хотя продюсеры решили, что Спирс слишком молода для участия в шоу, они представили её агенту в Нью-Йорке. Следующие 3 года Бритни училась в актёрской школе Professional Performing Arts School в Нью-Йорке и участвовала в нескольких постановках, в том числе «Ruthless!» 1991 года. В 1992 году Спирс участвовала в конкурсе Star Search, но проиграла во втором туре.\nВ 1993 году Спирс вернулась на канал «Дисней» и в течение 2-х лет участвовала в шоу «Новый Клуб Микки-Мауса». Другие будущие знаменитости, начинавшие с этого шоу — Кристина Агилера, участники 'N Sync Джастин Тимберлейк и Джейси Шазе, звезда сериала «Счастье» Кери Расселл и актёр фильма «Дневник памяти» Райан Гослинг.\nВ 1994 году шоу закрыли, Бритни вернулась домой в Луизиану, где поступила в среднюю школу. Некоторое время она пела в девичьей группе Innosense, но вскоре, решив начать сольную карьеру, записала демодиск, который попал в руки продюсерам из Jive Records, и те заключили с ней контракт.\nДалее последовал тур по стране, выступления в супермаркетах и работа на разогреве у групп 'N Sync и Backstreet Boys.\n[править]1999—2000: Ранний коммерческий успех\nВ октябре 1998 года вышел дебютный сингл Бритни Спирс «…Baby One More Time» . Песня имела огромный успех, в первые же недели возглавила международные чарты, мировые продажи сингла составили 9 миллионов копий, что сделало диск дважды платиновым. Альбом с одноимённым названием вышел в январе 1999 года. Альбом стартовал на первом месте рейтинга Billboard 200, пятьдесят одну неделю продержался в верхней десятке и шестьдесят недель в двадцати лучших. Альбом стал 15-кратным платиновым и на сегодняшний день является самым успешным альбомом Бритни Спирс.\nВ 1999 году Бритни снялась для апрельского номера журнала Rolling Stone. Откровенные фотографии спровоцировали слухи о том, что 17-летняя звезда сделала операцию по увеличению груди, что сама Спирс отрицала. Успех альбома и противоречивый образ Спирс, созданный массмедиа, сделали её главной звездой 1999 года.\nВслед за успешным дебютом последовал второй альбом певицы «Oops!... I Did It Again», также стартовавший на 1-м месте в США. Продажи за первую неделю составили 1 319 193 копии, что являлось абсолютным рекордом, который затем побил американский рэпер Эминем. Летом 2000 года Спирс отправилась в свой первый мировой тур, «Oops!… I Did It Again World Tour». В 2000 году Спирс получила две награды Billboards Music Awards и была номинирована на «Грэмми» в двух категориях — «Лучший поп-альбом» и «Лучшее живое выступление».\n[править]2001—2003: Вершина карьеры\n\n\nИсполняя «Me Against the Music»\nУспех Спирс сделал её заметной фигурой и в музыкальной индустрии, и в поп-культуре. В начале 2001 года она привлекла внимание «Пепси», эта компания предложила ей многомиллионный контракт, включавший телевизионную рекламу и участие в промо-акциях.\nВ ноябре 2001 года вышел третий альбом Спирс — Britney. Альбом дебютировал на первом месте в США с продажами в 745 744 пластинок за первую неделю, что сделало Бритни первой в истории исполнительницей, чьи первые три альбома стартовали на вершине рейтинга. Сразу же после выхода альбома Спирс отправилась в тур Dream Within a Dream Tour, по окончании которого объявила, что хочет взять 6-месячный перерыв в карьере.\nВ этом же году Спирс рассталась с солистом 'N Sync Джастином Тимберлейком, с которым встречалась 4 года.\nБритни вернулась на сцену в августе 2003 года.\nВ ноябре 2003 года вышел четвёртый студийный альбом Спирс In The Zone. Бритни участвовала в написании восьми из тринадцати композиций, а также выступила в качестве продюсера альбома. In The Zone дебютировал на первом месте в США, что сделало Бритни первой в истории исполнительницей, чьи первые четыре альбома стартовали на вершине рейтинга. Самый успешный сингл с альбома — Toxic — принёс Бритни первую для неё награду Грэмми в категории «Лучшая танцевальная композиция».\n[править]2007—2008: Возвращение к музыке\nВ начале 2007 года после двухлетнего перерыва Спирс приступила к записи нового сольного альбома, продюсерами которого выступили Nate «Danja» Hills, Шон Гарретт и Джонатан Ротэм.\nВ мае 2007 года Спирс в составе коллектива «The M and M’s» дала 6 концертов в рамках тура «House of Blues» в Лос-Анджелесе, Сан-Диего, Анахайме, Лас-Вегасе, Орландо и Майами. Каждый концерт длился около 15 минут и включал 5 старых хитов певицы.[4]\n30 августа 2007 года на волнах нью-йоркской радиостанции Z100 состоялась премьера песни «Gimme More», первого сингла с нового альбома Спирс.[5] Сингл вышел на iTunes 24 сентября и на CD 29 октября 2007.\n9 сентября 2007 года Спирс исполнила «Gimme More» на церемонии вручения наград MTV Video Music Awards. Выступление оказалось неудачным; Спирс выглядела непрофессионально — не всегда попадала в фонограмму и в танце отставала от группы хореографической поддержки.[6]\nНесмотря на это, в начале октября 2007 года сингл «Gimme More» достиг 3-го места в чарте Billboard Hot 100, став таким образом одним из самых успешных синглов Спирс.[7]"];
  return echo_factory_factory(protocol, messages);
};
factor_echo_special_chars = function(protocol) {
  var messages;
  messages = [" ", "\u0000", "\xff", "\xff\x00", "\x00\xff", " \r ", " \n ", " \r\n ", "\r\n", "", "message\t", "\tmessage", "message ", " message", "message\r", "\rmessage", "message\n", "\nmessage", "message\xff", "\xffmessage", "A", "b", "c", "d", "e", "\ufffd", "\ufffd\u0000", "message\ufffd", "\ufffdmessage"];
  return echo_factory_factory(protocol, messages);
};
factor_echo_large_message = function(protocol) {
  var messages;
  messages = [Array(Math.pow(2, 1)).join('x'), Array(Math.pow(2, 2)).join('x'), Array(Math.pow(2, 4)).join('x'), Array(Math.pow(2, 8)).join('x'), Array(Math.pow(2, 13)).join('x'), Array(Math.pow(2, 13)).join('x')];
  return echo_factory_factory(protocol, messages);
};
batch_factory_factory = function(protocol, messages) {
  return function() {
    var counter, r;
    expect(3 + messages.length);
    r = newSockJS('/echo', protocol);
    ok(r);
    counter = 0;
    r.onopen = function(e) {
      var msg, _i, _len, _results;
      ok(true);
      _results = [];
      for (_i = 0, _len = messages.length; _i < _len; _i++) {
        msg = messages[_i];
        _results.push(r.send(msg));
      }
      return _results;
    };
    r.onmessage = function(e) {
      equals(e.data, messages[counter]);
      counter += 1;
      if (counter === messages.length) {
        return r.close();
      }
    };
    return r.onclose = function(e) {
      if (counter !== messages.length) {
        ok(false, "Transport closed prematurely. " + e);
      } else {
        ok(true);
      }
      return start();
    };
  };
};
factor_batch_large = function(protocol) {
  var messages;
  messages = [Array(Math.pow(2, 1)).join('x'), Array(Math.pow(2, 2)).join('x'), Array(Math.pow(2, 4)).join('x'), Array(Math.pow(2, 8)).join('x'), Array(Math.pow(2, 13)).join('x'), Array(Math.pow(2, 13)).join('x')];
  return batch_factory_factory(protocol, messages);
};
batch_factory_factory_amp = function(protocol, messages) {
  return function() {
    var counter, r;
    expect(3 + messages.length);
    r = newSockJS('/amplify', protocol);
    ok(r);
    counter = 0;
    r.onopen = function(e) {
      var msg, _i, _len, _results;
      ok(true);
      _results = [];
      for (_i = 0, _len = messages.length; _i < _len; _i++) {
        msg = messages[_i];
        _results.push(r.send('' + msg));
      }
      return _results;
    };
    r.onmessage = function(e) {
      equals(e.data.length, Math.pow(2, messages[counter]), e.data);
      counter += 1;
      if (counter === messages.length) {
        return r.close();
      }
    };
    return r.onclose = function(e) {
      if (counter !== messages.length) {
        ok(false, "Transport closed prematurely. " + e);
      } else {
        ok(true);
      }
      return start();
    };
  };
};
factor_batch_large_amp = function(protocol) {
  var messages;
  messages = [1, 2, 4, 8, 13, 15, 15];
  return batch_factory_factory_amp(protocol, messages);
};
factor_user_close = function(protocol) {
  return function() {
    var counter, r;
    expect(4);
    r = newSockJS('/echo', protocol);
    ok(r);
    counter = 0;
    r.onopen = function(e) {
      counter += 1;
      ok(counter === 1);
      r.close(3000, "User message");
      return ok(counter === 1);
    };
    r.onmessage = function() {
      fail(true);
      return counter += 1;
    };
    return r.onclose = function(e) {
      counter += 1;
      log('user_close ' + e.status + ' ' + e.reason);
      ok(counter === 2);
      return start();
    };
  };
};
factor_server_close = function(protocol) {
  return function() {
    var r;
    expect(4);
    r = newSockJS('/close', protocol);
    ok(r);
    r.onopen = function(e) {
      return ok(true);
    };
    r.onmessage = function(e) {
      return fail(true);
    };
    return r.onclose = function(e) {
      equals(e.status, 3000);
      equals(e.reason, "Go away!");
      return start();
    };
  };
};
test_invalid_url_404 = function(protocol) {
  return function() {
    var counter, r;
    expect(2);
    r = newSockJS('/invalid_url', protocol);
    ok(r);
    counter = r.onopen = function(e) {
      return fail(true);
    };
    r.onmessage = function(e) {
      return fail(true);
    };
    return r.onclose = function(e) {
      log('404', e);
      equals(e.status, 2000);
      return start();
    };
  };
};
test_invalid_url_500 = function(protocol) {
  return function() {
    var r;
    expect(2);
    r = newSockJS('/500_error', protocol);
    ok(r);
    r.onopen = function(e) {
      return fail(true);
    };
    return r.onclose = function(e) {
      log('500', e);
      equals(e.status, 2000);
      return start();
    };
  };
};
test_invalid_url_port = function(protocol) {
  return function() {
    var dl, r;
    expect(2);
    dl = document.location;
    r = newSockJS(dl.protocol + '//' + dl.hostname + ':1079', protocol);
    ok(r);
    r.onopen = function(e) {
      return fail(true);
    };
    return r.onclose = function(e) {
      log('port', e);
      equals(e.status, 2000);
      return start();
    };
  };
};
arrIndexOf = function(arr, obj) {
  var i, _ref;
  for (i = 0, _ref = arr.length; 0 <= _ref ? i < _ref : i > _ref; 0 <= _ref ? i++ : i--) {
    if (arr[i] === obj) {
      return i;
    }
  }
  return -1;
};
test_protocol_messages = function(protocol) {
  module(protocol);
  if (!SockJS[protocol] || !SockJS[protocol].enabled(client_opts.sockjs_opts)) {
    return test("[unsupported by client]", function() {
      return log('Unsupported protocol (by client): "' + protocol + '"');
    });
  } else if (client_opts.disabled_transports && arrIndexOf(client_opts.disabled_transports, protocol) !== -1) {
    return test("[unsupported by server]", function() {
      return log('Unsupported protocol (by server): "' + protocol + '"');
    });
  } else {
    asyncTest("echo1", factor_echo_basic(protocol));
    asyncTest("echo2", factor_echo_rich(protocol));
    asyncTest("unicode", factor_echo_unicode(protocol));
    asyncTest("special_chars", factor_echo_special_chars(protocol));
    asyncTest("large message (ping-pong)", factor_echo_large_message(protocol));
    asyncTest("large message (batch)", factor_batch_large(protocol));
    asyncTest("large download", factor_batch_large_amp(protocol));
    asyncTest("user close", factor_user_close(protocol));
    return asyncTest("server close", factor_server_close(protocol));
  }
};
test_protocol_errors = function(protocol) {
  module(protocol);
  if (!SockJS[protocol] || !SockJS[protocol].enabled(client_opts.sockjs_opts)) {
    return test("[unsupported by client]", function() {
      return log('Unsupported protocol (by client): "' + protocol + '"');
    });
  } else if (client_opts.disabled_transports && arrIndexOf(client_opts.disabled_transports, protocol) !== -1) {
    return test("[unsupported by server]", function() {
      return log('Unsupported protocol (by server): "' + protocol + '"');
    });
  } else {
    asyncTest("invalid url 404", test_invalid_url_404(protocol));
    asyncTest("invalid url 500", test_invalid_url_500(protocol));
    return asyncTest("invalid url port", test_invalid_url_port(protocol));
  }
};
for (_i = 0, _len = protocols.length; _i < _len; _i++) {
  protocol = protocols[_i];
  test_protocol_messages(protocol);
}
module('other');
test("amending url", function() {
  var dl, r;
  dl = document.location;
  r = new SockJS('//blah:1/abc', []);
  equal(r._base_url, dl.protocol + '//blah:1/abc');
  r = new SockJS('/abc', []);
  equal(r._base_url, dl.protocol + '//' + dl.host + '/abc');
  r = new SockJS('http://a:1/abc', []);
  equal(r._base_url, 'http://a:1/abc');
  r = new SockJS('http://a:1/abc/', []);
  equal(r._base_url, 'http://a:1/abc');
  r = new SockJS('http://a:1/abc//', []);
  return equal(r._base_url, 'http://a:1/abc');
});
test("EventEmitter", function() {
  var bluff, r, single;
  expect(4);
  r = new SockJS('//blah/abc', []);
  r.addEventListener('message', function() {
    return ok(true);
  });
  r.onmessage = function() {
    return fail(true);
  };
  bluff = function() {
    return fail(true);
  };
  r.addEventListener('message', bluff);
  r.removeEventListener('message', bluff);
  r.addEventListener('message', bluff);
  r.addEventListener('message', function() {
    return ok(true);
  });
  r.onmessage = function() {
    return ok(true);
  };
  r.removeEventListener('message', bluff);
  r.dispatchEvent({
    type: 'message'
  });
  single = function() {
    return ok(true);
  };
  r.addEventListener('close', single);
  r.addEventListener('close', single);
  r.dispatchEvent({
    type: 'close'
  });
  r.removeEventListener('close', single);
  return r.dispatchEvent({
    type: 'close'
  });
});
chunking_test_factory = function(counter) {
  return function() {
    var a, go;
    expect(counter);
    a = new Array(counter);
    go = function() {
      return SockJS.chunkingTest(client_opts.url + '/echo', function(r) {
        if ($.browser.msie && $.browser.version < 8) {
          equal(r, false);
        } else {
          equal(r, true);
        }
        a.shift();
        if (a.length !== 0) {
          return go();
        } else {
          return start();
        }
      });
    };
    return go();
  };
};
asyncTest("chunking test (simple)", chunking_test_factory(1));
asyncTest("chunking test (stability)", chunking_test_factory(25));
asyncTest("chunking test, invalid url 404", function() {
  expect(1);
  return SockJS.chunkingTest(client_opts.url + '/invalid_url', function(r) {
    equal(r, false);
    return start();
  });
});
asyncTest("chunking test, invalid url 500", function() {
  expect(1);
  return SockJS.chunkingTest(client_opts.url + '/500_error', function(r) {
    equal(r, false);
    return start();
  });
});
asyncTest("chunking test, invalid url port", function() {
  var dl;
  expect(1);
  dl = document.location;
  return SockJS.chunkingTest(dl.protocol + '//' + dl.hostname + ':1079', function(r) {
    equal(r, false);
    return start();
  });
});
for (_j = 0, _len2 = protocols.length; _j < _len2; _j++) {
  protocol = protocols[_j];
  test_protocol_errors(protocol);
}