import scala.xml._

import org.apache.commons.lang3.StringEscapeUtils._

object enWordNetExtract {
  private final val plWordNetFile = "plWordNet-dev.xml.gz"

  def load_plwordnet(en : Boolean) = {
    val root = XML.load(new java.util.zip.GZIPInputStream(new java.io.FileInputStream(plWordNetFile)))
    val pwn_entries = (root \\ "lexical-unit").filter(x => en == (x \ "@pos").text.endsWith(" pwn")).map(
      x => (x \ "@id").text -> ((x \ "@name").text, (x \ "@pos").text)).toMap
    val descriptions = (root \\ "lexical-unit").filter(x => en == (x \ "@pos").text.endsWith(" pwn")).map(
      x => (x \ "@id").text -> (x \ "@desc").text).toMap
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
     (pwn_entries, synsets, lexrelations, synrelations, descriptions)
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
    case "3001" => "" // ???
    case "3002" => "" // ???
    case "3003" => "" // ???
    case "3004" => "" // ???
    case "3005" => "" // ???
    case "3006" => "" // ???
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

  sealed trait plWNDescription

  private final val simpleDefinition = "D: (.*)\\.?\\s*".r
  case class SimpleDefinition(text : String)

  private final val category = "K: (.*)\\.?\\s*".r
  case class Category(value : String)

  private final val otherSource = "(\\w+): (.*)\\.?\\s*".r
  case class OtherDefinition(source : String, text : String)

  private final val link = "L:\\s*(.*)\\.?\\s*".r
  case class Link(to : String)

  private final val ref = "<##REF\\d?:? ?(.*)>".r
  case class Ref(to : String)

  private final val emotion  ="A([123]):? ?(0|\\{\\s*([\\p{IsAlphabetic}, ]*)\\s*;\\s*([\\p{IsAlphabetic}, ]*)\\s*\\}\\s*(amb|0|\\+\\s*m|\\-\\s*m|\\+\\s*s|\\-\\s*s)\\s*\\[(.*)\\]\\s*)\\.?\\s*".r
  case class Emotion(annoNumber : Int,
    primary : Seq[EmotionValue], universal : Seq[EmotionValue],
    sentiment : Sentiment, example : String)

  sealed trait Sentiment
  object NoSent extends Sentiment
  object WeakPos extends Sentiment
  object StrongPos extends Sentiment
  object WeakNeg extends Sentiment
  object StrongNeg extends Sentiment
  object Ambivalent extends Sentiment
  object Sentiment {
    def fromString(s : String) = s match {
      case "0" => NoSent
      case "amb" => Ambivalent
      case "+m" => StrongPos
      case "+ m" => StrongPos
      case "-m" => StrongNeg
      case "- m" => StrongNeg
      case "-  m" => StrongNeg
      case "+s" => WeakPos
      case "+ s" => WeakPos
      case "-s" => WeakNeg
      case "- s" => WeakNeg
      case x =>
        System.err.println("Sentiment" + x)
        NoSent
    }
  }

  sealed trait EmotionValue
  sealed class PlutchikEmotion(val wnId : String) extends EmotionValue
  sealed trait UniversalEmotion extends EmotionValue
  object EmotionValue {
    object anticipation extends PlutchikEmotion("wn31-07526319-n")
    object joy extends PlutchikEmotion("wn31-07542591-n")
    object trust extends PlutchikEmotion("wn31-13952885-n")
    object fear extends PlutchikEmotion("wn31-07534492-n")
    object surprise extends PlutchikEmotion("wn31-07525587-n")
    object sadness extends PlutchikEmotion("wn31-07547828-n")
    object disgust extends PlutchikEmotion("wn31-07518499-n")
    object anger extends PlutchikEmotion("wn31-07531593-n")
    object beauty extends UniversalEmotion
    object ugliness extends UniversalEmotion
    object utility extends UniversalEmotion
    object uselessness extends UniversalEmotion
    object error extends UniversalEmotion
    object truth extends UniversalEmotion
    object wrong extends UniversalEmotion
    object goodness extends UniversalEmotion
    object ignorance extends UniversalEmotion
    object knowledge extends UniversalEmotion
    object happiness extends UniversalEmotion
    object unhappiness extends UniversalEmotion

    def fromPolish(s : String) : Seq[EmotionValue] = s.trim() match {
      // Universal
      case "bezużyteczność" => Seq(uselessness)
      case "bląd" => Seq(error)
      case "brzydota" => Seq(ugliness)
      case "brzygota" => Seq(ugliness)
      case "brzytoda" => Seq(ugliness)
      case "bład" => Seq(error)
      case "błąd" => Seq(error)
      case "dobro drugiego człowieka" => Seq(goodness)
      case "dobro drugiego" => Seq(goodness)
      case "dobro" => Seq(goodness)
      case "krzwda" => Seq(wrong)
      case "krzyada" => Seq(wrong)
      case "krzywda błąd" => Seq(wrong, error)
      case "krzywda" => Seq(wrong)
      case "niedzczęście" => Seq(sadness)
      case "nieiwedza" => Seq(ignorance)
      case "nieszczeście" => Seq(unhappiness)
      case "nieszczęscie" => Seq(unhappiness)
      case "nieszczęście" => Seq(unhappiness)
      case "nieszczęśćie" => Seq(unhappiness)
      case "nieurzyteczność" => Seq(uselessness)
      case "nieuzyteczność" => Seq(uselessness)
      case "nieużytecznosć" => Seq(uselessness)
      case "nieużytecznośc" => Seq(uselessness)
      case "nieużyteczność" => Seq(uselessness)
      case "nieużyteczność krzywda" => Seq(uselessness, wrong)
      case "niewiedza" => Seq(ignorance)
      case "nieżuyteczność" => Seq(uselessness)
      case "nieżyteczność" => Seq(uselessness)
      case "niużyteczność" => Seq(uselessness)
      case "piekno" => Seq(beauty)
      case "piękno" => Seq(beauty)
      case "prawda" => Seq(truth)
      //case "radość" => Seq(joy)
      //case "smutek" => Seq(sadness)
      case "sz częście" => Seq(happiness)
      case "szczeście" => Seq(happiness)
      //case "szczęscie" => Seq(happiness)
      case "szczęści" => Seq(happiness)
      case "szczęście użyteczność" => Seq(happiness, utility)
      //case "szczęście" => Seq(happiness)
      case "szczęśćie" => Seq(happiness)
      case "szczęscie" => Seq(happiness)
      case "uzyteczność" => Seq(utility)
      case "użuyteczność" => Seq(utility)
      case "użytecznosć" => Seq(utility)
      case "użytecznośc" => Seq(utility)
      case "użyteczność dobro" => Seq(goodness, utility)
      case "użyteczność szczęście" => Seq(utility, happiness)
      case "użyteczność wiedza" => Seq(utility, knowledge)
      case "użyteczność" => Seq(utility)
      case "wiedza" => Seq(knowledge)
      //case "wstręt" => Seq(disgust)
      //case "złość" => Seq(anger)
      // Primary
      case "cieszenie sie na" => Seq(anticipation)
      case "cieszenie sie" => Seq(anticipation)
      case "cieszenie się na" => Seq(anticipation)
      case "cieszenie się" => Seq(anticipation)
      case "ciesznie się na" => Seq(anticipation)
      case "gniew" => Seq(anger)
      //case "krzywda" => Seq(wrong)
      case "oczekiwanie na" => Seq(anticipation)
      case "radosć" => Seq(joy)
      case "radoć" => Seq(joy)
      case "radośc" => Seq(joy)
      case "radość" => Seq(joy)
      case "s mutek" => Seq(sadness)
      case "smitek" => Seq(sadness)
      case "smute" => Seq(sadness)
      case "smutek" => Seq(sadness)
      case "strach wstręt" => Seq(fear, disgust)
      case "strach" => Seq(fear)
      case "szczęście" => Seq(happiness)
      //case "użyteczność" => Seq(utility)
      case "wstret" => Seq(disgust)
      case "wstrę" => Seq(disgust)
      case "wstręt" => Seq(disgust)
      case "wstęt" => Seq(disgust)
      case "zaskoczenie" => Seq(surprise)
      case "zaufanie złość" => Seq(trust, anger)
      case "zaufanie" => Seq(trust)
      case "zlość" => Seq(anger)
      case "zufanie" => Seq(trust)
      case "złosć" => Seq(anger)
      case "złośc" => Seq(anger)
      case "złość" => Seq(anger)
      case "złóść" => Seq(anger)
      case "złość wstręt" => Seq(anger, disgust)
      case "" => Nil
      case _ =>
        System.err.println("Unrecognized emotion: " + s)
        Nil
    }
  }


  case class Error(test : String)

  val recognitionErrors = new java.io.PrintWriter("desc-errors.txt")
  var errorCount = 0

  def parseDescription(desc : String) = {
    def ri(i : Int) = if(i < 0) { Int.MaxValue -100} else { i }
    def elems(d : String) : List[String] = {
      val i1 = ri(d.indexOf("##"))
      val i2 = ri(d.indexOf("[##"))
      val i3 = ri(d.indexOf("{##"))
      val i4 = ri(d.indexOf("<"))
      if(i1 < i2 && i1 < i3 && i1 < i4 && i1 != Int.MaxValue) {
        if(i1 == 0) {
          elems(d.drop(2))
        } else {
          d.take(i1) :: elems(d.drop(i1 + 2))
        }
      } else if(i2 < i1 && i2 < i3 && i2 < i4 && i2 != Int.MaxValue) {
        val i = ri(d.indexOf("]", i2))
        if(i2 != 0) {
          d.take(i2) :: d.slice(i2+3,i) :: elems(d.drop(i+1))
        } else {
          d.slice(i2+3,i) :: elems(d.drop(i+1))
        }
      } else if(i3 < i1 && i3 < i2 && i3 < i4 && i3 != Int.MaxValue) {
        val i = ri(d.indexOf("}", i3))
        assert(i > i3)
        if(i3 != 0) {
          d.take(i3) :: d.slice(i3+3,i) :: elems(d.drop(i+1))
        } else {
          d.slice(i3+3,i) :: elems(d.drop(i+1))
        }
      } else if(i4 < i1 && i4 < i2 && i4 < i3 && i4 != Int.MaxValue) {
        val i = ri(d.indexOf(">", i4))
        if(i4 != 0) {
          d.take(i4) :: d.slice(i4,i+1) :: elems(d.drop(i+1))
        } else {
          d.slice(i4,i+1) :: elems(d.drop(i+1))
        }
      } else if(d matches "\\s*") {
        Nil
      } else {
        d :: Nil
      }
    }
    if(desc contains "##") {
      var hasError = false
      for(section <- elems(desc)) yield {
        section match {
          case simpleDefinition(text) =>
            Some(SimpleDefinition(text))
          case category(value) =>
            Some(Category(value))
          case link(to) =>
            Some(Link(to))
          case emotion(an, v, pes, ues, sent, ex) =>
            if(v == "0") {
              None
            } else {
              Some(Emotion(an.toInt, 
                pes.split(",\\s*").flatMap(EmotionValue.fromPolish), 
                ues.split(",\\s*").flatMap(EmotionValue.fromPolish), 
                Sentiment.fromString(sent), ex))
            }
          case ref(to) =>
            Some(Ref(to))
          case otherSource(source, text) =>
            Some(OtherDefinition(source, text))
          case "<##s>" => None //- compositional word combination
          case "<##aDD>" => None // - multi-word lexical unit -- convrt to <##DD>
          case "<##as1DD>" => None // - multi-word lexical unit exhibiting syntactic nonseparability
          case "<##as2DD>" => None // - multi-word lexical unit exhibiting fixed word order
          case "<##nDD>" => None // - compositional word combination
          case "<##DD>" => None //  ??
          case "<##DS>" => None //  ??
          case "<##sDD>" => None //  ??
          case "brak danych" => None // no data!
          case x => 
            if(!(x contains ":") && !(x contains "##") && !x.matches("\\s*")) {
              Some(SimpleDefinition(x))
            } else {
              if(!x.matches("\\s*")) {
                if(!hasError) {
                  recognitionErrors.println(desc)
                  hasError = true
                  errorCount += 1
                }
              }
              None
            }
        }
      }
    } else {
      Seq(Some(SimpleDefinition(desc)))
    }
  }
  def main(args : Array[String]) {
    {
      System.err.println("Extracting enWordNet")
      val (entries, synsets, lexrelations, synrelations, descriptions) = load_plwordnet(true)
      for(desc <- descriptions.values) {
        parseDescription(desc)
      }
      val (pwn_entries, pwn_defns, ili) = load_pwn

      val senses = build_senses(synsets)

      val (entry_mapping, synset_mapping) = map_plwordnet_to_pwn(entries, senses, synsets, pwn_entries, pwn_defns)

      print_wn(true, entries, synsets, senses, lexrelations, synrelations, entry_mapping,
        synset_mapping, ili)
    }
    {
      System.err.println("Extracting plWordNet")
      val (entries, synsets, lexrelations, synrelations, descriptions) = load_plwordnet(false)
      for(desc <- descriptions.values) {
        val p = parseDescription(desc)
      }
      System.err.println("Error Count: %d" format (errorCount))
      recognitionErrors.flush()

      val senses = build_senses(synsets)

      print_wn(false, entries, synsets, senses, lexrelations, synrelations, Map(), Map(), Map())
    }
  }
}
