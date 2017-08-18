$(function(){
  // Initialize Firebase
  var config = {
    apiKey: "AIzaSyDIHDclPRld6sPNo-0zmaizXXPEkBafnQc",
    authDomain: "quest2-40492.firebaseapp.com",
    databaseURL: "https://quest2-40492.firebaseio.com",
    projectId: "quest2-40492",
    storageBucket: "quest2-40492.appspot.com",
    messagingSenderId: "528554412315"
  };


  $("#data").mask("00/00/00");

  $("#hora_fim").mask("00:00");

  $("#hora_inicio").mask("00:00");


  firebase.initializeApp(config);
  var db = firebase.database().ref();


  function leva1(data, nome, hora_inicio, p1, p2, p3, imp1, imp2, imp3, sig_p1, sig_p2, sig_p3) {
    db.child(nome).set({
    data: data,
    hora_inicio: hora_inicio,
    ev1: p1,
    ev2: p2,
    ev3: p3,
    sig_ev1: sig_p1,
    sig_ev2: sig_p2,
    sig_ev3: sig_p3,
    imp_ev1: imp1,
    imp_ev2: imp2,
    imp_ev3: imp3
    });
  }




  $("#imp1").keyup(function(){
    var value = $(this).val().toString();
    var p1 = $("#p1").val().toString();
    if(value=="1"){
      $("#label_sig1").html("Significado da palavra <b>" + p1  + "</b>");
    }else if(value=="2"){
      $("#label_sig2").html("Significado da palavra "+ "<b>" + p1 + "</b>");
    }else if(value=="3"){
      $("#label_sig3").html("Significado da palavra " + "<b>" + p1 + "</b>");

    }
  });

  $("#imp2").keyup(function(){
    var value = $(this).val().toString();
    var p2 = $("#p2").val().toString();
    if(value=="1"){
      $("#label_sig1").html("Significado da palavra " + "<b>" +  p2 + "</b>");
    }else if(value=="2"){
      $("#label_sig2").html("Significado da palavra "+ "<b>" + p2 + "</b>");
    }else if(value=="3"){
      $("#label_sig3").html("Significado da palavra " + "<b>" + p2 + "</b>");
    }
  });


  $("#imp3").keyup(function(){
    var value = $(this).val().toString();
    var p3 = $("#p3").val().toString();
    if(value=="1"){
      $("#label_sig1").html("Significado da palavra " + "<b>" + p3  + "</b>");
    }else if(value=="2"){
      $("#label_sig2").html("Significado da palavra "+ "<b>" + p3 + "</b>");
    }else if(value=="3"){
      $("#label_sig3").html("Significado da palavra " + "<b>" + p3 + "</b>");

    }
  });

  $("#quadro1").submit(function(){
    var data = $("#data").val().toString();
    var nome = $("#nome_informante").val().toString();
    var inicio = $("#hora_inicio").val().toString();
    var p1 = $("#p1").val().toString();
    var p2 = $("#p2").val().toString();
    var p3 = $("#p3").val().toString();
    var imp1 = $("#imp1").val().toString();
    var imp2 = $("#imp2").val().toString();
    var imp3 = $("#imp3").val().toString();

    if(imp1 == "1"){
      var tp1 = p1;
      var timp1 = imp1;

    } else if (imp1 == "2") {
      var tp2 = p1;
      var timp2 = imp1;


    } else if(imp1==3) {
      var tp3 = p1;
      var timp3 = imp1;

    }


    if(imp2 == "1"){
      var tp1 = p2;
      var timp1 = imp2;


    } else if (imp2 == "2") {
      var tp2 = p2;
      var timp2 = imp2;


    } else if(imp2=="3"){
      var tp3 = p2;
      var timp3 = imp2;
    }


    if(imp3 == "1"){
      var tp1 = p3;
      var timp1 = imp3;
    } else if (imp3 == "2") {
      var timp2 = imp3;
      var tp2 = p3;
    } else {
      var timp3 = imp3;
      var tp3 = p3;
    }

    var sig_p1 = $("#sig_p1").val().toString();
    var sig_p2 = $("#sig_p2").val().toString();
    var sig_p3 = $("#sig_p3").val().toString();

    try{
      leva1(data, nome, inicio, tp1, tp2, tp3, timp1, timp2, timp3, sig_p1, sig_p2, sig_p3);

    }catch(err){
      alert(err);
    }

    $("#palavra1").html(tp1);
    $("#palavra2").html(tp2);
    $("#palavra3").html(tp3);
    $("#div_do_id").html(nome);

    var proximo = $("#quadro2").html();
    var now = $(this).html();
    $("#quadro1_again").html(now);
    $(".principal").hide().html(proximo).show("slow");
    $(".text-header").html("Esquema léxico");

    $("#exp1").html("EXPRESSÃO: " + tp1);
    $("#p1f_1").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é igual a <b>" + tp1 + "</b>?");
    $("#p2f_1").html("<b style='color:#990000;'>BARRAGEM DE FUNDÃO</b> é o contrário <b>" + tp1 + "</b>?");
    $("#p3f_1").html("<b style='color:#990000;'>BARRAGEM DE FUNDÃO</b> quer dizer <b>" + tp1 + "</b>?");


    $("#exp2").html("EXPRESSÃO: " + tp2);
    $("#p1f_2").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é igual a <b>" + tp2 + "</b>?");
    $("#p2f_2").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é o contrário <b>" + tp2 + "</b>?");
    $("#p3f_2").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> quer dizer <b>" + tp2 + "</b>?");

    $("#exp3").html("EXPRESSÃO: " + tp3);
    $("#p1f_3").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é igual a <b>" + tp3 + "</b>?");
    $("#p2f_3").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é o contrário <b>" + tp3 + "</b>?");
    $("#p3f_3").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> quer dizer <b>" + tp3 + "</b>?");



    return false;
  });


  function leva2(nome, el_ev1a, el_ev1b, el_ev1c, el_ev2a, el_ev2b, el_ev2c,el_ev3a, el_ev3b, el_ev3c) {
    db.child(nome).update({
    el_ev1a: el_ev1a,
    el_ev1b: el_ev1b,
    el_ev1c: el_ev1c,
    el_ev2a: el_ev2a,
    el_ev2b: el_ev2b,
    el_ev2c: el_ev2c,
    el_ev3a: el_ev3a,
    el_ev3b: el_ev3b,
    el_ev3c: el_ev3c
    });
  }

  $(document).on("click", "#subform2", function(){
    var nome = $("#div_do_id").html().toString();
    var tp1 = $("#palavra1").html().toString();
    var tp2 = $("#palavra2").html().toString();
    var tp3 = $("#palavra3").html().toString();

    var el_ev1a = $("input[name=el_ev1a]:checked").val().toString();
    var el_ev1b = $("input[name=el_ev1b]:checked").val().toString();
    var el_ev1c = $("input[name=el_ev1c]:checked").val().toString();

    var el_ev2a = $("input[name=el_ev2a]:checked").val().toString();
    var el_ev2b = $("input[name=el_ev2b]:checked").val().toString();
    var el_ev2c = $("input[name=el_ev2c]:checked").val().toString();

    var el_ev3a = $("input[name=el_ev3a]:checked").val().toString();
    var el_ev3b = $("input[name=el_ev3b]:checked").val().toString();
    var el_ev3c = $("input[name=el_ev3c]:checked").val().toString();






    try{
      leva2(nome, el_ev1a, el_ev1b, el_ev1c, el_ev2a, el_ev2b, el_ev2c,el_ev3a, el_ev3b, el_ev3c);

    }catch(err){
      alert(err);
    }

    var proximo = $("#quadro3").html();
    $(".principal").hide().html(proximo).show("slow");
    $(".text-header").html("Esquema de Vizinhança");

    $("#exp1_3").html("EXPRESSÃO: " + tp1);

    $("#p1f_1_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> faz parte de <b>" + tp1 + "</b>?");
    $("#p2f_1_tres").html("<b>" +tp1 + "</b> faz parte de <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p3f_1_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> e <b>" +tp1+ "</b> fazem parte do mesmo conceito?");

    $("#exp2_3").html("EXPRESSÃO: " + tp2);

    $("#p1f_2_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> faz parte de " + tp2 + "?");
    $("#p2f_2_tres").html("<b>"+tp2 + "</b> faz parte de <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p3f_2_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> e <b>" +tp2+ "</b> fazem parte do mesmo conceito?");

    $("#exp3_3").html("EXPRESSÃO: " + tp3);

    $("#p1f_3_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> faz parte de <b>" + tp1 + "</b>?");
    $("#p2f_3_tres").html("<b>"+tp1 + "</b> faz parte de <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p3f_3_tres").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> e <b>" +tp1+ "</b> fazem parte do mesmo conceito?");

  });

  function leva3(nome, ev_ev1a, ev_ev1b, ev_ev1c, ev_ev2a, ev_ev2b, ev_ev2c,ev_ev3a, ev_ev3b, ev_ev3c) {
    db.child(nome).update({
    ev_ev1a: ev_ev1a,
    ev_ev1b: ev_ev1b,
    ev_ev1c: ev_ev1c,
    ev_ev2a: ev_ev2a,
    ev_ev2b: ev_ev2b,
    ev_ev2c: ev_ev2c,
    ev_ev3a: ev_ev3a,
    ev_ev3b: ev_ev3b,
    ev_ev3c: ev_ev3c
    });
  }

  $(document).on("click", "#subform3", function(){
    var nome = $("#div_do_id").html().toString();
    var tp1 = $("#palavra1").html().toString();
    var tp2 = $("#palavra2").html().toString();
    var tp3 = $("#palavra3").html().toString();

    var ev_ev1a = $("input[name=ev_ev1a]:checked").val().toString();
    var ev_ev1b = $("input[name=ev_ev1b]:checked").val().toString();
    var ev_ev1c = $("input[name=ev_ev1c]:checked").val().toString();

    var ev_ev2a = $("input[name=ev_ev2a]:checked").val().toString();
    var ev_ev2b = $("input[name=ev_ev2b]:checked").val().toString();
    var ev_ev2c = $("input[name=ev_ev2c]:checked").val().toString();

    var ev_ev3a = $("input[name=ev_ev3a]:checked").val().toString();
    var ev_ev3b = $("input[name=ev_ev3b]:checked").val().toString();
    var ev_ev3c = $("input[name=ev_ev3c]:checked").val().toString();

    try{
      leva3(nome, ev_ev1a, ev_ev1b, ev_ev1c, ev_ev2a, ev_ev2b, ev_ev2c,ev_ev3a, ev_ev3b, ev_ev3c);

    }catch(err){
      alert(err);
    }

    var proximo = $("#quadro4").html();
    $(".principal").hide().html(proximo).show("slow");
    $(".text-header").html("Esquema prático");

    $("#exp1_4").html("EXPRESSÃO: " + tp1);

    $("#p1f_1_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> gera/cria <b>" + tp1 +  "</b>?");
    $("#p2f_1_quatro").html("O/A <b>" + tp1 +  "</b> é um meio utilizado pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> para causar consequências a algo ou alguém?");
    $("#p3f_1_quatro").html("O/A <b>" + tp1 + "</b> é uma ferramenta utilizada pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p4f_1_quatro").html("O/A <b>" + tp1 + "</b> é uma pessoa ou instituição que produz/cria a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p5f_1_quatro").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> depende de " +tp1 +  "?");
    $("#p6f_1_quatro").html("O/A <b>" + tp1 + "</b> é uma pessoa ou instituição que atua sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_1_quatro").html("O/A <b>" + tp1 + "</b> é uma ação exercida sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p8f_1_quatro").html("O/A <b>" +tp1 +  "</b> é um instrumento usado sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p9f_1_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é utilizada por <b>" + tp1 + "</b>?");
    $("#p10f_1_quatro").html("O/A <b>" + tp1+ "</b> é uma ação da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p11f_1_quatro").html("O/A <b>" +  tp1 +  "</b> é um objeto utilizado para gerar/criar a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");

    $("#exp2_4").html("EXPRESSÃO: " + tp2);
    $("#p1f_2_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> gera/cria " + tp2 +  "?");
    $("#p2f_2_quatro").html("O/A " + tp2 +  " é um meio utilizado pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> para causar consequências a algo ou alguém?");
    $("#p3f_2_quatro").html("O/A " + tp2 + " é uma ferramenta utilizada pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p4f_2_quatro").html("O/A " + tp2 + " é uma pessoa ou instituição que produz/cria a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p5f_2_quatro").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> depende de " +tp2 +  "?");
    $("#p6f_2_quatro").html("O/A " + tp2 + " é uma pessoa ou instituição que atua sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_2_quatro").html("O/A " + tp2 + " é uma ação exercida sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p8f_2_quatro").html("O/A " +tp2 +  " é um instrumento usado sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p9f_2_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é utilizada por " + tp2 + "?");
    $("#p10f_2_quatro").html("O/A " + tp2+ " é uma ação da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p11f_2_quatro").html("O/A" +  tp2 +  " é um objeto utilizado para gerar/criar a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");

    $("#exp3_4").html("EXPRESSÃO: " + tp3);
    $("#p1f_3_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> gera/cria <b>" + tp3 +  "</b>?");
    $("#p2f_3_quatro").html("O/A <b>" + tp3 +  " </b>é um meio utilizado pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> para causar consequências a algo ou alguém?");
    $("#p3f_3_quatro").html("O/A <b>" + tp3 + "</b> é uma ferramenta utilizada pela <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p4f_3_quatro").html("O/A <b>" + tp3 + "</b> é uma pessoa ou instituição que produz/cria a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p5f_3_quatro").html("<b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> depende de <b>" +tp3 +  "</b>?");
    $("#p6f_3_quatro").html("O/A <b>" + tp3 + "</b> é uma pessoa ou instituição que atua sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_3_quatro").html("O/A <b>" + tp3 + "</b> é uma ação exercida sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p8f_3_quatro").html("O/A <b>" + tp3 +  "</b> é um instrumento usado sobre a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p9f_3_quatro").html("A <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> é utilizada por <b>" + tp3 + "</b>?");
    $("#p10f_3_quatro").html("O/A <b>" + tp3+ "</b> é uma ação da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p11f_3_quatro").html("O/A <b>" +  tp3 +  "</b> é um objeto utilizado para gerar/criar a <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");

  });



  $(document).on("click", "#subform4", function(){

    var tp1 = $("#palavra1").html().toString();
    var tp2 = $("#palavra2").html().toString();
    var tp3 = $("#palavra3").html().toString();
    var nome = $("#div_do_id").html().toString();

    var ep_ev1a = $("input[name=ep_ev1a]:checked").val().toString();
    var ep_ev1b = $("input[name=ep_ev1b]:checked").val().toString();
    var ep_ev1c = $("input[name=ep_ev1c]:checked").val().toString();
    var ep_ev1d = $("input[name=ep_ev1d]:checked").val().toString();
    var ep_ev1e = $("input[name=ep_ev1e]:checked").val().toString();
    var ep_ev1f = $("input[name=ep_ev1f]:checked").val().toString();
    var ep_ev1g = $("input[name=ep_ev1g]:checked").val().toString();
    var ep_ev1h = $("input[name=ep_ev1h]:checked").val().toString();
    var ep_ev1i = $("input[name=ep_ev1i]:checked").val().toString();
    var ep_ev1j = $("input[name=ep_ev1j]:checked").val().toString();
    var ep_ev1k = $("input[name=ep_ev1k]:checked").val().toString();


    var ep_ev2a = $("input[name=ep_ev2a]:checked").val().toString();
    var ep_ev2b = $("input[name=ep_ev2b]:checked").val().toString();
    var ep_ev2c = $("input[name=ep_ev2c]:checked").val().toString();
    var ep_ev2d = $("input[name=ep_ev2d]:checked").val().toString();
    var ep_ev2e = $("input[name=ep_ev2e]:checked").val().toString();
    var ep_ev2f = $("input[name=ep_ev2f]:checked").val().toString();
    var ep_ev2g = $("input[name=ep_ev2g]:checked").val().toString();
    var ep_ev2h = $("input[name=ep_ev2h]:checked").val().toString();
    var ep_ev2i = $("input[name=ep_ev2i]:checked").val().toString();
    var ep_ev2j = $("input[name=ep_ev2j]:checked").val().toString();
    var ep_ev2k = $("input[name=ep_ev2k]:checked").val().toString();

    var ep_ev3a = $("input[name=ep_ev3a]:checked").val().toString();
    var ep_ev3b = $("input[name=ep_ev3b]:checked").val().toString();
    var ep_ev3c = $("input[name=ep_ev3c]:checked").val().toString();
    var ep_ev3d = $("input[name=ep_ev3d]:checked").val().toString();
    var ep_ev3e = $("input[name=ep_ev3e]:checked").val().toString();
    var ep_ev3f = $("input[name=ep_ev3f]:checked").val().toString();
    var ep_ev3g = $("input[name=ep_ev3g]:checked").val().toString();
    var ep_ev3h = $("input[name=ep_ev3h]:checked").val().toString();
    var ep_ev3i = $("input[name=ep_ev3i]:checked").val().toString();
    var ep_ev3j = $("input[name=ep_ev3j]:checked").val().toString();
    var ep_ev3k = $("input[name=ep_ev3k]:checked").val().toString();
    try{
      db.child(nome).update({
      ep_ev1a: ep_ev1a,
      ep_ev1b: ep_ev1b,
      ep_ev1c: ep_ev1c,
      ep_ev1d: ep_ev1d,
      ep_ev1e: ep_ev1e,
      ep_ev1f: ep_ev1f,
      ep_ev1g: ep_ev1g,
      ep_ev1h: ep_ev1h,
      ep_ev1i: ep_ev1i,
      ep_ev1j: ep_ev1j,
      ep_ev1k: ep_ev1k,
      ep_ev2a: ep_ev2a,
      ep_ev2b: ep_ev2b,
      ep_ev2c: ep_ev2c,
      ep_ev2d: ep_ev2d,
      ep_ev2e: ep_ev2e,
      ep_ev2f: ep_ev2f,
      ep_ev2g: ep_ev2g,
      ep_ev2h: ep_ev2h,
      ep_ev2i: ep_ev2i,
      ep_ev2j: ep_ev2j,
      ep_ev2k: ep_ev2k,
      ep_ev3a: ep_ev3a,
      ep_ev3b: ep_ev3b,
      ep_ev3c: ep_ev3c,
      ep_ev3d: ep_ev3d,
      ep_ev3e: ep_ev3e,
      ep_ev3f: ep_ev3f,
      ep_ev3g: ep_ev3g,
      ep_ev3h: ep_ev3h,
      ep_ev3i: ep_ev3i,
      ep_ev3j: ep_ev3j,
      ep_ev3k: ep_ev3k
    });

    }catch(err){
      alert(err);

    }


    var proximo = $("#quadro5").html();
    $(".principal").hide().html(proximo).show("slow");
    $(".text-header").html("Esquema atribuição");


    $("#exp1_5").html("EXPRESSÃO: " + tp1);

    $("#p1f_1_cinco").html("Todas as vezes (sempre) que você pensa em <p>"+tp1+"</b>, você considera que esta palavra é uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p2f_1_cinco").html("Você considera frequentemente <b>" +tp1+ "</b> como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p3f_1_cinco").html("Você considera <b>" +tp1+"</b> apenas de vez em quando como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p4f_1_cinco").html("Você considera <b>" +tp1+ "</b> como uma qualidade da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p5f_1_cinco").html("Se você pensa em <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> você já consegue prever <b>"  +tp1+"</b> ocorrendo?");
    $("#p6f_1_cinco").html("Você considera que <b>"  +tp1+ "</b> é causa da existência da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_1_cinco").html("Você considera que a <b>"+tp1+ "</b> é consequência (resultado) da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");

    $("#exp2_5").html("EXPRESSÃO: " + tp2);

    $("#p1f_2_cinco").html("Todas as vezes (sempre) que você pensa em <b>"+tp2+"</b>, você considera que esta palavra é uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p2f_2_cinco").html("Você considera frequentemente <b>" +tp2+ "</b> como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p3f_2_cinco").html("Você considera <b>" +tp2+"</b> apenas de vez em quando como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p4f_2_cinco").html("Você considera <b>" +tp2+ "</b> como uma qualidade da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p5f_2_cinco").html("Se você pensa em <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> você já consegue prever <b>"  +tp2+"</b> ocorrendo?");
    $("#p6f_2_cinco").html("Você considera que <b>"  +tp2+ "</b> é causa da existência da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_2_cinco").html("Você considera que a <b>"+tp2+ "</b> é consequência (resultado) da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");

    $("#exp3_5").html("EXPRESSÃO: " + tp3);

    $("#p1f_3_cinco").html("Todas as vezes (sempre) que você pensa em <b>"+tp3+"</b>, você considera que esta palavra é uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p2f_3_cinco").html("Você considera frequentemente <b>" +tp3+ "</b> como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p3f_3_cinco").html("Você considera <b>" +tp3+"</b> apenas de vez em quando como uma característica da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p4f_3_cinco").html("Você considera <b>" +tp3+ "</b> como uma qualidade da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> ?");
    $("#p5f_3_cinco").html("Se você pensa em <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b> você já consegue prever <b>"  +tp3+"</b> ocorrendo?");
    $("#p6f_3_cinco").html("Você considera que <b>"  +tp3+ "</b> é causa da existência da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");
    $("#p7f_3_cinco").html("Você considera que a <b>"+tp3+ "</b> é consequência (resultado) da <b style='color: #990000;'>BARRAGEM DE FUNDÃO</b>?");




  });

  $(document).on("click", "#subform5", function(){
    var tp1 = $("#palavra1").html().toString();
    var tp2 = $("#palavra2").html().toString();
    var tp3 = $("#palavra3").html().toString();
    var nome = $("#div_do_id").html().toString();

    var ea_ev1a = $("input[name=ea_ev1a]:checked").val().toString();
    var ea_ev1b = $("input[name=ea_ev1b]:checked").val().toString();
    var ea_ev1c = $("input[name=ea_ev1c]:checked").val().toString();
    var ea_ev1d = $("input[name=ea_ev1d]:checked").val().toString();
    var ea_ev1e = $("input[name=ea_ev1e]:checked").val().toString();
    var ea_ev1f = $("input[name=ea_ev1f]:checked").val().toString();
    var ea_ev1g = $("input[name=ea_ev1g]:checked").val().toString();

    var ea_ev2a = $("input[name=ea_ev2a]:checked").val().toString();
    var ea_ev2b = $("input[name=ea_ev2b]:checked").val().toString();
    var ea_ev2c = $("input[name=ea_ev2c]:checked").val().toString();
    var ea_ev2d = $("input[name=ea_ev2d]:checked").val().toString();
    var ea_ev2e = $("input[name=ea_ev2e]:checked").val().toString();
    var ea_ev2f = $("input[name=ea_ev2f]:checked").val().toString();
    var ea_ev2g = $("input[name=ea_ev2g]:checked").val().toString();

    var ea_ev3a = $("input[name=ea_ev3a]:checked").val().toString();
    var ea_ev3b = $("input[name=ea_ev3b]:checked").val().toString();
    var ea_ev3c = $("input[name=ea_ev3c]:checked").val().toString();
    var ea_ev3d = $("input[name=ea_ev3d]:checked").val().toString();
    var ea_ev3e = $("input[name=ea_ev3e]:checked").val().toString();
    var ea_ev3f = $("input[name=ea_ev3f]:checked").val().toString();
    var ea_ev3g = $("input[name=ea_ev3g]:checked").val().toString();
    var hora_fim = $("#hora_fim").val().toString();

    $("#hora_fim").mask("00:00");


    try{
      db.child(nome).update({
      ea_ev1a: ea_ev1a,
      ea_ev1b: ea_ev1b,
      ea_ev1c: ea_ev1c,
      ea_ev1d: ea_ev1d,
      ea_ev1e: ea_ev1e,
      ea_ev1f: ea_ev1f,
      ea_ev1g: ea_ev1g,

      ea_ev2a: ea_ev2a,
      ea_ev2b: ea_ev2b,
      ea_ev2c: ea_ev2c,
      ea_ev2d: ea_ev2d,
      ea_ev2e: ea_ev2e,
      ea_ev2f: ea_ev2f,
      ea_ev2g: ea_ev2g,

      ea_ev3a: ea_ev3a,
      ea_ev3b: ea_ev3b,
      ea_ev3c: ea_ev3c,
      ea_ev3d: ea_ev3d,
      ea_ev3e: ea_ev3e,
      ea_ev3f: ea_ev3f,
      ea_ev3g: ea_ev3g,
      hora_fim: hora_fim
    });

    }catch(err){
      alert(err);

    }

    var proximo = $("#quadro6").html();
    $(".principal").hide().html(proximo).show("slow");
    $(".text-header").html("");


  });


  $(document).on("click", "#reiniciar", function(){
    location.reload();

  });


  //$("input[name=name_of_your_radiobutton]:checked").val();

});
