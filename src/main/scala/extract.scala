import scala.xml._

import org.apache.commons.lang3.StringEscapeUtils._

object enWordNetExtract {

  def load_plwordnet(en : Boolean) = {
    val root = XML.load(new java.util.zip.GZIPInputStream(new java.io.FileInputStream("plwordnet_2_3.xml.gz")))
    val pwn_entries = (root \\ "lexical-unit").filter(x => en == (x \ "@pos").text.endsWith(" pwn")).map(
      x => (x \ "@id").text -> ((x \ "@name").text, (x \ "@pos").text)).toMap
    val synsets = (root \\ "synset").filter(
      x => (x \ "unit-id").exists(n => pwn_entries contains n.text)).map(
      x => (x \ "@id").text -> ((x \ "unit-id").map(_.text), (x \ "@definition").text)).toMap
    val lexrelations = (root \\ "lexicalrelations").filter(
      x => (synsets contains (x \ "@parent").text) && (synsets contains (x \ "@child").text)).map(
      x => ((x \ "@parent").text, (x \ "@child").text, (x \ "@relation").text)).
      groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))
    val synrelations = (root \\ "synsetrelations").filter(
      x => (synsets contains (x \ "@parent").text) && (synsets contains (x \ "@child").text)).map(
      x => ((x \ "@parent").text, (x \ "@child").text, (x \ "@relation").text)).
      groupBy(_._1).mapValues(_.map(x => (x._2, x._3)))
     (pwn_entries, synsets, lexrelations, synrelations)
  }

  def build_senses(synsets : Map[String, (Seq[String], String)]) : Map[String, Seq[String]] = {
    synsets.flatMap({
      case (sid, (lids, _)) =>
        lids.map(_ -> sid)
    }).groupBy(_._1).mapValues(_.map(_._2).toSeq)
  }

  def polishToPos(polish : String) = polish match {
    case "przymiotnik pwn" => "a"
    case "przymiotnik" => "a"
    case "rzeczownik pwn"  => "n"
    case "rzeczownik"  => "n"
    case "czasownik pwn"   => "v"
    case "czasownik"   => "v"
    case "przysłówek pwn"  => "r"
    case "przysłówek"  => "r"
  }

  def mapRelType(relType : String) = relType match {
    // Relations only in plWordNet
    case "10" => relType
    case "11" => relType
    case "13" => relType
    case "19" => relType
    case "20" => relType
    case "21" => relType
    case "22" => relType
    case "23" => relType
    case "24" => relType
    case "25" => relType
    case "26" => relType
    case "27" => relType
    case "28" => relType
    case "29" => relType
    case "34" => relType
    case "35" => relType
    case "36" => relType
    case "37" => relType
    case "38" => relType
    case "39" => relType
    case "40" => relType
    case "41" => relType
    case "42" => relType
    case "43" => relType
    case "44" => relType
    case "45" => relType
    case "46" => relType
    case "47" => relType
    case "48" => relType
    case "49" => relType
    case "50" => relType
    case "51" => relType
    case "52" => relType
    case "53" => relType
    case "55" => relType
    case "56" => relType
    case "57" => relType
    case "58" => relType
    case "59" => relType
    case "60" => relType
    case "62" => relType
    case "63" => relType
    case "64" => relType
    case "65" => relType
    case "74" => relType
    case "75" => relType
    case "89" => relType
    case "90" => relType
    case "92" => relType
    case "93" => relType
    case "101" => relType
    case "104" => relType
    case "106" => relType
    case "107" => relType
    case "108" => relType
    case "110" => relType
    case "111" => relType
    case "113" => relType
    case "114" => relType
    case "116" => relType
    case "117" => relType
    case "118" => relType
    case "119" => relType
    case "120" => relType
    case "121" => relType
    case "122" => relType
    case "124" => relType
    case "125" => relType
    case "126" => relType
    case "127" => relType
    case "129" => relType
    case "130" => relType
    case "131" => relType
    case "134" => relType
    case "136" => relType
    case "137" => relType
    case "138" => relType
    case "140" => relType
    case "141" => relType
    case "142" => relType
    case "145" => relType
    case "146" => relType
    case "147" => relType
    case "148" => relType
    case "149" => relType
    case "151" => relType
    case "152" => relType
    case "154" => relType
    case "155" => relType
    case "156" => relType
    case "157" => relType
    case "158" => relType
    case "160" => relType
    case "161" => relType
    case "163" => relType
    case "164" => relType
    case "165" => relType
    case "166" => relType
    case "168" => relType
    case "169" => relType
    case "194" => relType
    case "201" => relType
    case "202" => relType
    case "203" => relType
    case "205" => relType
    case "206" => relType
    case "207" => relType
    case "211" => relType
    case "212" => relType
    case "214" => relType
    case "215" => relType
    case "216" => relType
    case "217" => relType
    case "218" => relType
    case "219" => relType
    case "225" => relType
    case "226" => relType
    case "228" => relType
    case "229" => relType
    case "230" => relType
    case "235" => relType
    case "238" => relType
    case "239" => relType
    case "242" => relType
    case "244" => relType
    case "3000" => relType
    // Relations in both resources
    case "170" => "antonym"
    case "171" => "hyponym"
    case "172" => "instance_hyponym"
    case "173" => "hypernym"
    case "174" => "instance_hypernym"
    case "175" => "member_meronym"
    case "176" => "substance_meronym"
    case "177" => "part_meronym"
    case "178" => "member_holonym"
    case "179" => "substance_holonym"
    case "180" => "part_holonym"
    case "181" => "attribute"
    case "182" => "derivation"
    case "183" => "domain_member_category"
    case "184" => "domain_category"
    case "185" => "domain_member_region"
    case "186" => "domain_region"
    case "187" => "domain_member_usage"
    case "188" => "domain_usage"
    case "189" => "189"
    case "190" => "190"
    case "191" => "191"
    case "192" => "192"
    case "193" => "similar"
    case "195" => "195"
    case "208" => "208"
    case "209" => "209"
    case "210" => "210"
    case "213" => "213"
    case "222" => "222"
    case "223" => "223"
  }

  def load_pwn : (Map[String, Seq[String]], Map[String, Seq[String]], Map[String, String]) = {
    val wordnet = xml.XML.load(new java.util.zip.GZIPInputStream(new java.io.FileInputStream("wn31.xml.gz")))

    val lemma_synsets = (wordnet \\ "LexicalEntry").map(x => ((x \ "Lemma" \ "@writtenForm").text,
                                                              (x \ "Sense").map(s => (s \ "@synset").text)))

    val synset_definitions = (wordnet \\ "Synset").map(x => ((x \ "@id").text, (x \ "Definition").text, (x \ "@ili").text))


    (lemma_synsets.groupBy(_._1).mapValues(_.flatMap(_._2)),
      synset_definitions.groupBy(_._1).mapValues(_.map(_._2).toSeq),
      synset_definitions.map(x => x._1 -> x._3).toMap)
  }

  def map_plwordnet_to_pwn(entries : Map[String, (String, String)],
                           senses : Map[String, Seq[String]],
                           synsets : Map[String, (Seq[String], String)],
                           pwn_entries : Map[String, Seq[String]],
                           pwn_defns : Map[String, Seq[String]]) :
    (Map[String, String], Map[String, String]) = {
    val entryMapping = entries.map({
      case (plwn_eid, (lemma, _)) =>
         val plwn_defns = senses.getOrElse(plwn_eid, Nil).map(plwn_sid =>
             synsets(plwn_sid)._2.trim())
        val pwn_sids = pwn_entries.getOrElse(lemma, Nil).flatMap({
          pwn_sid => 
            if(pwn_defns.getOrElse(pwn_sid, Nil).exists({ pwn_defn =>
              plwn_defns contains pwn_defn
            })) {
              Seq(pwn_sid)
            } else {
              Seq()
            }
        })
        pwn_sids match {
          case Seq() =>
            plwn_eid -> "in"
          case Seq(pwn_sid) =>
            plwn_eid -> pwn_sid
          case _ =>
            System.err.println("Ambiguous mapping for " + plwn_eid)
            plwn_eid -> pwn_sids.head
        }
    })
    val synsetMapping = entryMapping.flatMap({
      case (plwn_eid, pwn_sid) =>
        senses.getOrElse(plwn_eid, Nil).map({
          plwn_sid =>
            plwn_sid -> pwn_sid
        })
    }).toMap
    (entryMapping.filter(_._2 != "in"), synsetMapping.filter(_._2 != "in"))
  }

  def print_wn(en : Boolean,
               entries : Map[String, (String, String)],
               synsets : Map[String, (Seq[String], String)],
               senses  : Map[String, Seq[String]],
               lexrelations : Map[String, Seq[(String, String)]],
               synrelations : Map[String, Seq[(String, String)]],
               entry_mapping : Map[String, String],
               synset_mapping : Map[String, String],
               ili : Map[String, String]) = {
    val entries_grouped : Map[(String, String), Map[String, (String, String)]] = 
        entries.groupBy(x => x._2)
    val fileName = if(en) { "enwordnet.xml.gz" } else { "plwordnet.xml.gz" }
    val out = new java.io.PrintWriter(new java.util.zip.GZIPOutputStream(new java.io.FileOutputStream(fileName)))
    if(en) {
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
  <Lexicon id="enwordnet"
           label="enWordNet"
           language="en" 
           email="plwordnet.pwr.wroc.pl@gmail.com"
           license="http://nlp.pwr.wroc.pl/plwordnet/license"
           version="2.3"
           url="http://nlp.pwr.wroc.pl/plwordnet/">""")
    } else {
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
<LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
  <Lexicon id="plwordnet"
           label="plWordNet"
           language="pl" 
           email="plwordnet.pwr.wroc.pl@gmail.com"
           license="http://nlp.pwr.wroc.pl/plwordnet/license"
           version="2.3"
           url="http://nlp.pwr.wroc.pl/plwordnet/">""")
    }
    for(((name,pos), group) <- entries_grouped) {
      out.print(s"""
      <LexicalEntry id="${name}">
        <Lemma writtenForm="${name}"
               partOfSpeech="${polishToPos(pos)}"/>""")
      for(entryid <- group.keys) {
        for(sense <- senses.getOrElse(entryid, Nil)) {
          out.print(s"""
        <Sense id="${entryid}"
               synset="${entry_mapping.getOrElse(entryid, "pl-" + sense)}">""")
          for((targ, relType) <- lexrelations.getOrElse(entryid, Nil)) {
            val tid = entry_mapping.getOrElse(targ, "pl-" + targ)
            out.print(s"""
          <SenseRelation target="${tid}" relType="${mapRelType(relType)}"/>""")
          }
          out.print(s"""
        </Sense>""")
        }
      }
      out.print("""
      </LexicalEntry>""")
    }
    for((synsetid, (_, defn)) <- synsets) {
      val sid = synset_mapping.getOrElse(synsetid, "pl-" + synsetid)
      val iliid = if(en) { ili.getOrElse(sid, "in") } else { "" }
      out.print(s"""
      <Synset id="${sid}" ili="${iliid}"> 
        <Definition>${defn}</Definition>""")
      for((targ, rel) <- synrelations.getOrElse(synsetid, Nil)) {
        val tid = synset_mapping.getOrElse(targ, "pl-" + targ)
        out.print(s"""
        <SynsetRelation target="${tid}" relType="${mapRelType(rel)}"/>""")
      }
      out.println("""
      </Synset>""")
    }
    out.println("""
    </Lexicon>
  </LexicalResource>""")
  }

  def main(args : Array[String]) {
    {
      System.err.println("Extracting enWordNet")
      val (entries, synsets, lexrelations, synrelations) = load_plwordnet(true)
      val (pwn_entries, pwn_defns, ili) = load_pwn

      val senses = build_senses(synsets)

      val (entry_mapping, synset_mapping) = map_plwordnet_to_pwn(entries, senses, synsets, pwn_entries, pwn_defns)

      print_wn(true, entries, synsets, senses, lexrelations, synrelations, entry_mapping,
        synset_mapping, ili)
    }
    {
      System.err.println("Extracting plWordNet")
      val (entries, synsets, lexrelations, synrelations) = load_plwordnet(false)

      val senses = build_senses(synsets)

      print_wn(false, entries, synsets, senses, lexrelations, synrelations, Map(), Map(), Map())
    }
  }
}
