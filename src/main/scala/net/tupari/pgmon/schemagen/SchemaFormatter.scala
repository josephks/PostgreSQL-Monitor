package net.tupari.pgmon.schemagen

/**
 * Created by IntelliJ IDEA.
 * User: jks
 * Date: 1/30/12
 * Time: 9:05 PM
 */

sealed abstract class SchemaFormatter( writer: java.io.PrintWriter) {
    def format(tableDef: TableDef)
}

private object SchemaFormatHelpers{
  val timestampRegex = "timestamp.*".r
  val timeRegex = "time .*".r
  val charsRegex = """character\((\d+)\)""".r
}

class LiftFormatter( writer: java.io.PrintWriter) extends SchemaFormatter(writer){
  def format(tableDef: TableDef){
    val classname = net.liftweb.util.HttpHelpers.camelify( tableDef.name )
    writer.print("class " + classname +" " );
    tableDef.getPkField match{
      case Some(pkfield) =>
        if (pkfield.fieldType == "integer" ||  pkfield.fieldType == "longint")
          writer.print("extends LongKeyedMapper["+classname+"]" )
        if (pkfield.name == "id")
          writer.print("with IdPk")
      case None =>
        writer.println("//table has no primary key, cannot be mapped")
    }
    writer.println(" {")
    tableDef.fields.foreach{
      field =>
        writer.append("    object ").append(field.name).append(" extends Mapped")
        field.fieldType match{
          case "integer" => "Int"
          case "bigint" => "Long"
          case "text" => "Text"
          case x => "Unknown[" + x + "]"
          //still work to do here
        }

    }
    writer.println("}")
    writer.println()
  }
}

class JpaFormatter(writer: java.io.PrintWriter) extends  SchemaFormatter(writer){
  def format(tableDef: TableDef){
    val classname = net.liftweb.util.HttpHelpers.camelify( tableDef.name )
    writer.println("@Entity")
    writer.print("@Table(name=\"" + tableDef.name+"\"" )
    val uniques = for( index <- tableDef.indexes if index.unique)
       yield "@UniqueConstraint(columnNames={\"" + index.fields.map(f => f.name).mkString("\",\"") +"\"})"

    if (! uniques.isEmpty)
          writer.println(",\n    uniqueConstraints=("+ uniques.mkString(",\n      ")+")")

    writer.println( ")")
    writer.println("public class " + classname +"  implements Serializable {" );
    tableDef.fields.foreach{
      field =>
        field.constraints.foreach{
          case Constraint.Pkey => writer.println("   @Id")
          case _ =>
        }
        field.fieldType match{
          case  SchemaFormatHelpers.charsRegex(numchars) =>
            writer.println("   @Size(max=" + numchars +")")
          case "date" =>
            writer.println("   @Temporal(javax.persistence.TemporalType.DATE)")
          case        SchemaFormatHelpers.timestampRegex() =>
            writer.println("   @Temporal(javax.persistence.TemporalType.TIMESTAMP)")
          case        SchemaFormatHelpers.timeRegex() =>
            writer.println("   @Temporal(javax.persistence.TemporalType.TIME)")
          case _ =>
        }
        if (field.notnull)
             writer.println("   @NotNull")
        writer.append("   protected ").append(
          field.fieldType match{
            case "integer" => "Integer"
            case "smallint" => "Short"
            case "bigint" => "Long"
            case "text" => "String"
            case "boolean" => "Boolean"
            case  SchemaFormatHelpers.charsRegex(_) => "String"
            case  SchemaFormatHelpers.timestampRegex() => "java.util.Date"
            case "date" => "java.util.Date"
            case  SchemaFormatHelpers.timeRegex  () => "java.util.Date"
            case x => "Unknown[" + x + "]"
          } ).append(" ").append(field.name).append(";")
        writer.println()
        writer.println()
    } //foreach field
    //now write foreign key mappings
       def oneOrMany(uniq:Boolean) = if (uniq) "One" else "Many"

    tableDef.outgoingForeignKeys.foreach{  fk =>
    val joinColDec = if (fk.from.length == 1) "@JoinColumn(name=\"" + fk.from(0).name + "\") " else {
      "@JoinColumns({ " + fk.from.zip(fk.to).map {
        case (fromf, tof) => "@JoinColumn(name=\"" + fromf.name + "\", referencedColumnName=\"" + tof.name + "\")"
      }.mkString(",\n       ") +" })"
    }
    writer.println("   @" + oneOrMany( fk.fromUniq) + "To" + oneOrMany( fk.toUniq) +" "+ joinColDec)
    writer.println("   " +
      (if (! fk.toUniq) "Collection<" else "" )+
      net.liftweb.util.HttpHelpers.camelify( fk.totable.name )  +
      (if (! fk.toUniq) "> " else " ")+
      fk.totable.name +";" )
    writer.println()
    writer.println()

    } //foreach
    writer.println("}")
    writer.println()
  } //def format
}

class TableLister ( writer: java.io.PrintWriter) extends   SchemaFormatter(writer){
  def format(tableDef: TableDef){
        writer.println(tableDef.name)
  }
}
