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
      x => ((x \ "@child").text, (x \ "@parent").text, (x \ "@relation").text)).
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

    val coreSynRels = Set("agent", "also", "attribute", "be_in_state", "causes", "classified_by", "classifies", "co_agent_instrument", "co_agent_patient", "co_agent_result", "co_instrument_agent", "co_instrument_patient", "co_instrument_result", "co_patient_agent", "co_patient_instrument", "co_result_agent", "co_result_instrument", "co_role", "derivation", "direction", "domain_region", "domain_topic", "domain_usage", "entails", "eq_synonym", "fuzzynym", "has_domain_region", "has_domain_topic", "has_domain_usage", "holo_location", "holo_member", "holo_part", "holo_portion", "holo_substance", "holonym", "hypernym", "hyponym", "in_manner", "instance_hypernym", "instance_hyponym", "instrument", "involved", "involved_agent", "involved_direction", "involved_instrument", "involved_location", "involved_patient", "involved_result", "involved_source_direction", "involved_target_direction", "is_caused_by", "is_entailed_by", "location", "manner_of", "mero_location", "mero_member", "mero_part", "mero_portion", "mero_substance", "meronym", "near_synonym", "other", "patient", "restricted_by", "restricts", "result", "role", "source_direction", "state_of", "target_direction")
    val coreSenseRels = Set("antonym", "near_antonym", "also", "participle", "pertainym", "derivation", "domain_topic", "has_domain_topic", "domain_region", "has_domain_region", "domain_usage", "has_domain_usage", "other")

  def mapRelType(relType : String) = relType match {
    // Relations only in plWordNet
    case "10" => "hyponym"
    case "11" => "hypernym"
    case "13" => "conversion"
    case "19" => "fuzzynym"
    case "20" => "mero_part"
    case "21" => "mero_portion"
    case "22" => "mero_location"
    case "23" => "mero_member"
    case "24" => "mero_substance"
    case "25" => "holo_part"
    case "26" => "holo_portion"
    case "27" => "holo_location"
    case "28" => "holo_member"
    case "29" => "holo_substance"
    case "34" => "agent"
    case "35" => "patient"
    case "36" => "instrument"
    case "37" => "location"
    case "38" => "result"
    case "39" => "time"
    case "40" => "role"
    case "41" => "involved_agent"
    case "42" => "involved_patient"
    case "43" => "involved_time"
    case "44" => "involved_location"
    case "45" => "involved_instrument"
    case "46" => "involved_result"
    case "47" => "involved"
    case "48" => "agent_hidden"
    case "49" => "location_hidden"
    case "50" => "result_hidden"
    case "51" => "be_in_state"
    case "52" => "state"
    case "53" => "feminine"
    case "55" => "young"
    case "56" => "diminutive"
    case "57" => "augmentative"
    case "58" => "inhabitant"
    case "59" => "derivation"
    case "60" => "inter_register_synonym"
    case "62" => "noun_verb_cross_category_synonym"
    case "63" => "noun_adjective_cross_category_synonym"
    case "64" => "taxonomic_meronym"
    case "65" => "taxonomic_holonym"
    case "74" => "pure_perfective_imperfective"
    case "75" => "pure_imperfective_perfective"
    case "89" => "imperfective_verb_adjective_processuality"
    case "90" => "imperfective_verb_noun_processuality"
    case "92" => "verb_adjective_state"
    case "93" => "verb_noun_state"
    case "101" => "complementary_antonym"
    case "104" => "proper_antonym"
    case "106" => "type"
    case "107" => "instance_hyponym"
    case "108" => "synset_fuzzynym"
    case "110" => "secondary_aspect_perfective_imperfective"
    case "111" => "secondary_aspect_imperfective_perfective"
    case "113" => "meronym_of_substitution"
    case "114" => "meronym_of_accompanying_situation"
    case "116" => "holonym_of_substitution"
    case "117" => "holonym_of_accompanying_situation"
    case "118" => "verb_perfective_adjective_processuality"
    case "119" => "verb_perfective_noun_processuality"
    case "120" => "cause_imperfective_imperfective"
    case "121" => "cause_perfective_perfective"
    case "122" => "perfective_imperfective_cause_of_state"
    case "124" => "perfective_adjective_cause_of_process"
    case "125" => "perfective_noun_cause_of_process"
    case "126" => "imperfective_adjective_cause_of_process"
    case "127" => "imperfective_noun_cause_of_process"
    case "129" => "perfective_imperfective"
    case "130" => "imperfective_imperfective"
    case "131" => "reflexive_meaning"
    case "134" => "iterative_imperfective_imperfective"
    case "136" => "distributive"
    case "137" => "presuppositional"
    case "138" => "preceding"
    case "140" => "iterative_imperfective_perfective"
    case "141" => "verb_noun_cross_category_synonym"
    case "142" => "adjective_noun_cross_category_synonym"
    case "145" => "value_of_attribute"
    case "146" => "modifier"
    case "147" => "gradation"
    case "148" => "similititudinal_meaning"
    case "149" => "characterizing"
    case "151" => "comparative"
    case "152" => "superlative"
    case "154" => "agent"
    case "155" => "patient"
    case "156" => "instrument"
    case "157" => "location"
    case "158" => "time"
    case "160" => "result"
    case "161" => "cause"
    case "163" => "potential"
    case "164" => "habitual"
    case "165" => "quantitative"
    case "166" => "evaluation"
    case "168" => "markedness_of_intensity"
    case "169" => "cross_category_synonym_for_relational_adjectives"
    case "194" => "participle_of_verb"
    case "201" => "holo_part"
    case "202" => "holo_member"
    case "203" => "holo_madeof"
    case "205" => "mero_part"
    case "206" => "mero_member"
    case "207" => "mero_substance"
    case "211" => "hypernym"
    case "212" => "hyponym"
    case "214" => "holo_part"
    case "215" => "holo_member"
    case "216" => "holo_substance"
    case "217" => "mero_part"
    case "218" => "mero_member"
    case "219" => "mero_madeof"
    case "225" => "near_synonym"
    case "226" => "near_synonym"
    case "228" => "inter_register_synonym"
    case "229" => "inter_register_synonym"
    case "230" => "instance_hypernym" // To Sumo
    case "235" => ""
    case "238" => ""
    case "239" => ""
    case "242" => ""
    case "244" => ""
    case "3000" => "" // ???
    // Relations in both resources
    case "170" => "antonym"
    case "171" => "hyponym"
    case "172" => "instance_hyponym"
    case "173" => "hypernym"
    case "174" => "instance_hypernym"
    case "175" => "mero_member"
    case "176" => "mero_substance"
    case "177" => "mero_part"
    case "178" => "holo_member"
    case "179" => "holo_substance"
    case "180" => "holo_part"
    case "181" => "attribute"
    case "182" => "derivation"
    case "183" => "has_domain_topic"
    case "184" => "domain_topic"
    case "185" => "has_domain_region"
    case "186" => "domain_region"
    case "187" => "has_domain_usage"
    case "188" => "domain_usage"
    case "189" => "entails"
    case "190" => "causes"
    case "191" => "also"
    case "192" => "fuzzynym"
    case "193" => "fuzzynym"
    case "195" => "pertainym"
    case "208" => "eq_synonym" // eq_synonym2
    case "209" => "eq_synonym" // eq_synonym3
    case "210" => "hypernym" // hypernym
    case "213" => "hyponym" // hyponym
    case "222" => "" // automatic_prompt2
    case "223" => "" // automatic_prompt3
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
            val t = mapRelType(relType)
            if(coreSenseRels contains t) {
              out.print(s"""
          <SenseRelation target="${tid}" relType="$t"/>""")
            } else if(t != "") {
              out.print(s"""
          <SenseRelation target="${tid}" relType="other" dc:type="$t"/>""")
            }
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
        val t = mapRelType(rel)
        if(coreSynRels contains t) {
          out.print(s"""
        <SynsetRelation target="${tid}" relType="$t"/>""")
        } else if(t != "") {
          out.print(s"""
        <SynsetRelation target="${tid}" relType="other" dc:type="$t"/>""")
        }
      }
      out.println("""
      </Synset>""")
    }
    out.println("""
    </Lexicon>
  </LexicalResource>""")
    out.flush
    out.close
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
