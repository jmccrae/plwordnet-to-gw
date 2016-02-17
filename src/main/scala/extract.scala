import scala.xml._

import org.apache.commons.lang3.StringEscapeUtils._

object enWordNetExtract {

  def load_plwordnet = {
    val root = XML.loadFile("plwordnet_2_3.xml")
    val pwn_entries = (root \\ "lexical-unit").filter(x => (x \ "@pos").text.endsWith(" pwn")).map(
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
    case "rzeczownik pwn"  => "n"
    case "czasownik pwn"   => "v"
    case "przysłówek pwn"  => "r"
  }

  def mapRelType(relType : String) = relType match {
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

  def load_pwn : (Map[String, Seq[String]], Map[String, Seq[String]]) = {
    val wordnet = xml.XML.loadFile("/home/jmccrae/projects/globalwordnet/wndb2lmf/wn31.xml")

    val lemma_synsets = (wordnet \\ "LexicalEntry").map(x => ((x \ "Lemma" \ "@writtenForm").text,
                                                              (x \ "Sense").map(s => (s \ "@synset").text)))

    val synset_definitions = (wordnet \\ "Synset").map(x => ((x \ "@id").text, (x \ "Definition").text))


    (lemma_synsets.groupBy(_._1).mapValues(_.flatMap(_._2)),
      synset_definitions.groupBy(_._1).mapValues(_.map(_._2).toSeq))
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

  def print_wn(entries : Map[String, (String, String)],
               synsets : Map[String, (Seq[String], String)],
               senses  : Map[String, Seq[String]],
               lexrelations : Map[String, Seq[(String, String)]],
               synrelations : Map[String, Seq[(String, String)]],
               entry_mapping : Map[String, String],
               synset_mapping : Map[String, String]) = {
    val entries_grouped : Map[(String, String), Map[String, (String, String)]] = 
        entries.groupBy(x => x._2)
    val out = new java.io.PrintWriter("enwordnet.xml")
    out.print("""<?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE LexicalResource SYSTEM "http://globalwordnet.github.io/schemas/WN-LMF-1.0.dtd">
    <LexicalResource xmlns:dc="http://purl.org/dc/elements/1.1/">
      <Lexicon id="enwordnet"
               label="enWordNet"
               language="en" 
               email="plwordnet.pwr.wroc.pl@gmail.com"
               license="http://nlp.pwr.wroc.pl/plwordnet/license"
               version="0.16.2"
               url="http://nlp.pwr.wroc.pl/plwordnet/">""")
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
        out.print("""
      </LexicalEntry>""")
      }
    }
    for((synsetid, (_, defn)) <- synsets) {
      val sid = synset_mapping.getOrElse(synsetid, "pl-" + synsetid)
      out.print(s"""
      <Synset id="${sid}" ili=""> 
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
    val (entries, synsets, lexrelations, synrelations) = load_plwordnet
    val (pwn_entries, pwn_defns) = load_pwn

    val senses = build_senses(synsets)

    val (entry_mapping, synset_mapping) = map_plwordnet_to_pwn(entries, senses, synsets, pwn_entries, pwn_defns)

    print_wn(entries, synsets, senses, lexrelations, synrelations, entry_mapping,
      synset_mapping)
  }
}
