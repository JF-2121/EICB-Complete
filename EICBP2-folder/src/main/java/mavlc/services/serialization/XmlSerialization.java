/*******************************************************************************
 * Copyright (c) 2016-2019 Embedded Systems and Applications Group
 * Department of Computer Science, Technische Universitaet Darmstadt,
 * Hochschulstr. 10, 64289 Darmstadt, Germany.
 * <p>
 * All rights reserved.
 * <p>
 * This software is provided free for educational use only.
 * It may not be used for commercial purposes without the
 * prior written permission of the authors.
 ******************************************************************************/
package mavlc.services.serialization;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.converters.reflection.ReflectionConverter;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;
import com.thoughtworks.xstream.io.xml.PrettyPrintWriter;
import com.thoughtworks.xstream.io.xml.StaxDriver;
import mavlc.syntax.AstNode;
import mavlc.syntax.SourceLocation;
import mavlc.syntax.expression.*;
import mavlc.syntax.function.FormalParameter;
import mavlc.syntax.function.Function;
import mavlc.syntax.module.Module;
import mavlc.syntax.record.RecordElementDeclaration;
import mavlc.syntax.record.RecordTypeDeclaration;
import mavlc.syntax.statement.*;
import mavlc.syntax.type.*;
import mavlc.type.*;

import java.io.*;
import java.lang.reflect.Field;
import java.nio.file.Path;

public class XmlSerialization {
	private XmlSerialization() { }
	
	private static final XStream xstream;
	
	static {
		xstream = new XStream(new StaxDriver());
		
		xstream.omitField(Throwable.class, "stackTrace");
		xstream.omitField(Throwable.class, "suppressedExceptions");
		
		// include line and column attributes in node tag
		xstream.registerConverter(new AstNodeConverter(xstream));
		xstream.omitField(AstNode.class, "sourceLocation");
		
		// simple types are all immutable (avoid unnecessary references in xml)
		xstream.addImmutableType(BoolType.class, false);
		xstream.addImmutableType(IntType.class, false);
		xstream.addImmutableType(FloatType.class, false);
		xstream.addImmutableType(StringType.class, false);
		xstream.addImmutableType(VoidType.class, false);
		
		xstream.omitField(Type.class, "wordSize");
		xstream.omitField(TypeSpecifier.class, "typeClass");
		
		xstream.useAttributeFor(String.class);
		xstream.useAttributeFor(Integer.class);
		xstream.useAttributeFor(Float.class);
		xstream.useAttributeFor(Boolean.class);
		xstream.useAttributeFor(int.class);
		xstream.useAttributeFor(float.class);
		xstream.useAttributeFor(boolean.class);
		
		Class<?>[] astClasses = new Class[]{
				// types
				BoolType.class,
				FloatType.class,
				IntType.class,
				MatrixType.class,
				VectorType.class,
				RecordType.class,
				StringType.class,
				VoidType.class,
				// type specifiers
				BoolTypeSpecifier.class,
				FloatTypeSpecifier.class,
				IntTypeSpecifier.class,
				MatrixTypeSpecifier.class,
				VectorTypeSpecifier.class,
				RecordTypeSpecifier.class,
				StringTypeSpecifier.class,
				VoidTypeSpecifier.class,
				// miscellaneous
				Module.class,
				Function.class,
				FormalParameter.class,
				RecordTypeDeclaration.class,
				RecordElementDeclaration.class,
				// boolean operations
				And.class,
				Or.class,
				Not.class,
				Compare.class,
				// arithmetic operations
				Addition.class,
				Subtraction.class,
				Multiplication.class,
				Division.class,
				Exponentiation.class,
				UnaryMinus.class,
				DotProduct.class,
				MatrixMultiplication.class,
				MatrixTranspose.class,
				// structure access
				StructureInit.class,
				RecordInit.class,
				ElementSelect.class,
				SubVector.class,
				SubMatrix.class,
				RecordElementSelect.class,
				VectorDimension.class,
				MatrixRows.class,
				MatrixCols.class,
				// literals
				BoolValue.class,
				FloatValue.class,
				IntValue.class,
				StringValue.class,
				// other expressions
				CallExpression.class,
				SelectExpression.class,
				// identifiers
				IdentifierReference.class,
				LeftHandIdentifier.class,
				VectorLhsIdentifier.class,
				MatrixLhsIdentifier.class,
				RecordLhsIdentifier.class,
				// declarations
				Declaration.class,
				ValueDefinition.class,
				VariableDeclaration.class,
				IteratorDeclaration.class,
				// statements
				VariableAssignment.class,
				CompoundStatement.class,
				CallStatement.class,
				ForLoop.class,
				ForEachLoop.class,
				IfStatement.class,
				SwitchStatement.class,
				Case.class,
				Default.class,
				ReturnStatement.class,
		};
		
		for(Class<?> astClass : astClasses) {
			xstream.alias(astClass.getSimpleName(), astClass);
		}

		xstream.allowTypes(astClasses);
	}
	
	public static <T> void serialize(T obj, Path outPath) {
		try {
			BufferedWriter fileWriter = new BufferedWriter(new FileWriter(outPath.toFile()));
			HierarchicalStreamWriter streamWriter = new PrettyPrintWriter(fileWriter);
			xstream.marshal(obj, streamWriter);
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static <T> T deserialize(String fileName) {
		try {
			BufferedReader reader = new BufferedReader(new FileReader(fileName));
			// noinspection unchecked
			return (T) xstream.fromXML(reader);
		} catch(FileNotFoundException e) {
			throw new RuntimeException(e);
		}
	}
	
	public static <T> String serialize(T obj) {
		return xstream.toXML(obj);
	}
	
	/** This XStream converter is used to inline line and column information as attributes */
	private static class AstNodeConverter extends ReflectionConverter {
		public AstNodeConverter(XStream xstream) {
			super(xstream.getMapper(), xstream.getReflectionProvider());
		}
		
		@Override public void marshal(Object original, HierarchicalStreamWriter writer, MarshallingContext context) {
			AstNode node = (AstNode) original;
			writer.addAttribute("line", Integer.toString(node.sourceLocation.line));
			writer.addAttribute("column", Integer.toString(node.sourceLocation.column));
			super.marshal(original, writer, context);
		}
		
		@Override public Object unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
			String lineAttr = reader.getAttribute("line");
			String columnAttr = reader.getAttribute("column");
			int line = lineAttr != null ? Integer.parseInt(lineAttr) : -1;
			int column = columnAttr != null ? Integer.parseInt(columnAttr) : -1;
			
			try {
				AstNode node = (AstNode) super.unmarshal(reader, context);
				Field field = AstNode.class.getField("sourceLocation");
				field.setAccessible(true);
				field.set(node, new SourceLocation(line, column));
				field.setAccessible(false);
				return node;
			} catch(NoSuchFieldException | IllegalAccessException e) {
				throw new RuntimeException(e);
			} catch(Exception e) {
				e.printStackTrace();
				throw e;
			}
		}
		
		@Override public boolean canConvert(Class type) {
			return AstNode.class.isAssignableFrom(type);
		}
	}
}
